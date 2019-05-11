module Components.Admin.MeetingManager where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Components.Admin.SpeakerQueue as SQ
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.List as L
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import FormHelpers (setFocus)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Postgrest as PG
import Types.Agenda (Agenda, AgendaItem(..))
import Types.Agenda as AG
import Types.Attendee (AttendeeDB)
import Types.Flash as FL
import Types.Token (Token)
import Web.Event.Event (Event, preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State =
  { agenda    :: Agenda
  , token     :: Token
  , attendees :: AttendeeDB
  }

type Input = State

data Query a
  = Init                   a
  | PreviousAI             a
  | NextAI                 a
  | SQMsg       SQ.Message a
  | GotNewState State      a
  | HandleKey   Event      a

derive instance functorQuery :: Functor Query

data Message
  = Flash (Maybe FL.Flash)

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.lifecycleParentComponent
    { initialState: identity
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: HE.input GotNewState
    }
  where
    render :: State -> H.ParentHTML Query SQ.Query Unit Aff
    render {agenda, attendees, token} =
      case AG.getCurrentAI agenda of
        Left e ->
          HH.div_ [HH.text e]
        Right ai@(AgendaItem {id, speakerQueues, title, parent}) ->
          HH.div_
            [ HH.div
               [HP.id_ "agenda-item-buttons"]
               [ HH.button
                  [ HE.onClick (HE.input_ PreviousAI), HP.id_ "prev-button" ]
                  [ HH.text "⇐" ]
               , HH.div
                   [ HP.id_ "ai-headings" ]
                   (maybe
                     [HH.h1_ [HH.text title]]
                     (\(AgendaItem pai) -> [HH.h1_ [HH.text pai.title], HH.h2_ [HH.text title]])
                     parent)
               , HH.button
                 [ HE.onClick (HE.input_ NextAI), HP.id_ "next-button" ]
                 [ HH.text "⇒" ]
               ]
             , case AG.topSQ ai of
                 Nothing ->
                   HH.text "We have no speaker queues I'm afraid. This shouldn't happen. It happened anyway."
                 Just speakerQueue ->
                   HH.slot
                     unit
                     SQ.component
                     { speakerQueue
                     , token
                     , attendees
                     , agendaItemId: id
                     , sqHeight: L.length speakerQueues
                     }
                     (HE.input SQMsg)
             ]

    eval :: Query ~> H.ParentDSL State Query SQ.Query Unit Message Aff
    eval =
      case _ of
        Init next -> do
          document <- H.liftEffect $ Web.document =<< Web.window
          focusId
          H.subscribe $ ES.eventSource
            (\f -> do
              el <- eventListener (f <<< H.action <<< HandleKey)
              addEventListener (KET.keydown) el true (HTMLDocument.toEventTarget document)
            )
            (pure <<< map (const ES.Listening))
          pure next
        PreviousAI next ->
          H.raise (Flash Nothing) *> step AG.prev *> pure next
        NextAI next ->
          H.raise (Flash Nothing) *> step AG.next *> pure next
        SQMsg (SQ.Flash m) next ->
          H.raise (Flash m) *> pure next
        GotNewState s next ->
          H.put s *> pure next
        HandleKey ev next ->
          let p  = const (H.liftEffect $ preventDefault ev)
              go =
                case _ of
                  "n" -> p unit *> H.query unit (H.action SQ.Next) *> pure next  -- Next speaker
                  "e" -> p unit *> H.query unit (H.action SQ.Eject) *> pure next -- Eject speaker
                  "h" -> p unit *> eval (PreviousAI next) -- Previous agenda item
                  "l" -> p unit *> eval (NextAI next)     -- Next agenda item
                  _   -> pure next
           in sequence (fromEvent ev <#> (key >>> go)) *> pure next

    focusId = H.liftEffect (setFocus "id")

    step
      :: (Agenda -> Maybe Agenda)
      -> H.HalogenM State Query SQ.Query Unit Message Aff Unit
    step f =
      H.get
        >>= \{agenda} ->
            setCurrentAgendaItem
              ((note "ERROR: Agenda: Out of bounds step." (f agenda)) >>= AG.getCurrentAI)
        *> focusId

    setCurrentAgendaItem
      :: Either String AgendaItem
      -> H.HalogenM State Query SQ.Query Unit Message Aff Unit
    setCurrentAgendaItem =
      case _ of
        Left s ->
          H.raise $ Flash $ Just $ FL.mkFlash s FL.Error
        Right (AgendaItem c) -> do
          {token} <- H.get
          {status} <- H.liftAff (PG.emptyResponse
                 (PG.createURL "/rpc/set_current_agenda_item")
                 token
                 POST
                 {id: c.id})
          case status of
            StatusCode 200 ->
              pure unit
            _ ->
              H.raise $ Flash $ Just $
                FL.mkFlash
                  "setCurrentAgendaItem -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
                  FL.Error

