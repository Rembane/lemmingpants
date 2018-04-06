module Components.Admin where

import Types

import Components.SpeakerQueue as SQ
import Control.Monad.Aff (Aff)
import Data.Either (Either(..), either, note)
import Data.Foldable (foldMap)
import Data.Foreign (renderForeignError)
import Data.HTTP.Method (Method(..))
import Data.Map as M
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (mempty)
import Data.Sequence.Ordered as OrdSeq
import Data.Tuple (snd)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Postgrest as PG
import Prelude (type (~>), Unit, bind, const, id, map, pure, show, unit, (*>), (<#>), (<<<), (<>), (==), (>>=))
import Queue (modifyCurrent)
import Queue as Q
import Simple.JSON (readJSON)

type Agenda = Q.Queue AgendaItem

type State =
  { agenda    :: Agenda
  , token     :: Maybe String
  , attendees :: M.Map Int Attendee
  }
type Input = State

data Query a
  = PreviousAI       a
  | NextAI           a
  | SQMsg SQ.Message a

data Message
  = Flash          String
  | ModifiedAgenda Agenda

component :: forall e. H.Component HH.HTML Query Input Message (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState: \{token, agenda, attendees} ->
        { agenda:    agenda
        , token:     token
        , attendees: attendees
        }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query SQ.Query Unit (Aff (LemmingPantsEffects e))
    render state =
      HH.div_
        ([ HH.h1_ [HH.text "Supreme council interface"]
        , HH.h2_
          [ HH.button
            [ HE.onClick (HE.input_ PreviousAI) ]
            [ HH.text "⇐" ]
          , HH.text " "
          , HH.text (either id id (map (\(AgendaItem a) -> a.title) currentAI))
          , HH.text " "
          , HH.button
            [ HE.onClick (HE.input_ NextAI) ]
            [ HH.text "⇒" ]
          ]
        ] <> either
            (const [HH.text "No agenda item => no speaker queue."])
            (\(AgendaItem ai) ->
              [ HH.p_ [ HH.text ("Speaker queue stack height: " <> show (OrdSeq.length ai.speakerQueues)) ]
              , case OrdSeq.greatest ai.speakerQueues of
                  Nothing -> HH.text "We have no speaker queues I'm afraid. This shouldn't happen. It happened anyway."
                  Just sq ->
                    HH.slot
                      unit
                      SQ.component
                      { speakerQueue: sq, token: state.token, attendees: state.attendees, agendaItemId: ai.id }
                      (HE.input SQMsg)
              ])
            currentAI
        )
      where
        currentAI = getCurrentAI state.agenda

    eval :: Query ~> H.ParentDSL State Query SQ.Query Unit Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        PreviousAI next ->
          (H.gets (\s -> s.agenda) >>= stepQueue Q.prev) *> pure next
        NextAI next ->
          (H.gets (\s -> s.agenda) >>= stepQueue Q.next) *> pure next
        SQMsg m next ->
          case m of
            SQ.Flash m' ->
              H.raise (Flash m')
            SQ.Push  sq ->
              H.gets (\s -> s.agenda)
                <#> modifyCurrent
                  (\(AgendaItem a) -> AgendaItem (a { speakerQueues = OrdSeq.insert sq a.speakerQueues }))
                >>= (H.raise <<< ModifiedAgenda)
            SQ.Pop      ->
              H.gets (\s -> s.agenda)
                <#> modifyCurrent
                  (\(AgendaItem a) -> AgendaItem (a { speakerQueues = maybe mempty snd (OrdSeq.popGreatest a.speakerQueues) }))
                >>= (H.raise <<< ModifiedAgenda)
            SQ.ModifiedTop sq ->
              H.gets (\s -> s.agenda)
                <#> modifyCurrent (\(AgendaItem a) ->
                  AgendaItem (a { speakerQueues = OrdSeq.insert sq (maybe mempty snd (OrdSeq.popGreatest a.speakerQueues)) }))
                >>= (H.raise <<< ModifiedAgenda)
          *> pure next

    getCurrentAI :: Q.Queue AgendaItem -> Either String AgendaItem
    getCurrentAI =
      note "ERROR: There is no current agenda item." <<< Q.getCurrent

    stepQueue
      :: (Q.Queue AgendaItem -> Q.Queue AgendaItem)
      -> Q.Queue AgendaItem
      -> H.HalogenM State Query SQ.Query Unit Message (Aff (LemmingPantsEffects e)) Unit
    stepQueue f q =
      let q' = f q
       in if q == q'
            then pure unit
            else setCurrentAgendaItem q'

    setCurrentAgendaItem
      :: Q.Queue AgendaItem
      -> H.HalogenM State Query SQ.Query Unit Message (Aff (LemmingPantsEffects e)) Unit
    setCurrentAgendaItem newQueue =
      case getCurrentAI newQueue of
        Left s ->
          H.raise (Flash s)
        Right (AgendaItem c) -> do
          s  <- H.get
          er <- H.liftAff (PG.signedInAjax
                  "http://localhost:3000/rpc/set_current_agenda_item"
                  s.token
                  POST
                  mempty
                  {id: c.id})
          case er of
            Left  m -> H.raise (Flash m)
            Right r -> do
              case readJSON r.response of
                Left es -> H.raise (Flash (foldMap renderForeignError es))
                Right i' ->
                  if i' == c.id
                    then H.modify (_ {agenda = newQueue })
                    else H.raise (Flash ("The backend didn't set the wanted agenda item as the current agenda item. This is very strange. Wanted ID: " <> show c.id <> ", actual ID: " <> show i'))
