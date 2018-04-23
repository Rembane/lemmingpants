module Components.Overhead where

import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe, maybe')
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Prelude (type (~>), Void, id, map, pure, (*>), (<>))
import Types.Agenda (Agenda, AgendaItem(AgendaItem))
import Types.Agenda as AG
import Types.Attendee (AttendeeDB)
import Types.Speaker (visualizeSpeaker)
import Types.SpeakerQueue (SpeakerQueue(..))

type State =
  { agenda    :: Agenda
  , attendees :: AttendeeDB
  }
type Input = State

data Query a = GotNewState Input a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input GotNewState
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      case currentAI of
        Left  e                   ->
          HH.text e
        Right ai@(AgendaItem ai') ->
          HH.div_
            (case ai'.supertitle of
              Nothing -> [ HH.h1 [HP.id_ "with-ruler"] [HH.text ai'.title] ]
              Just st -> [ HH.h1 [HP.id_ "with-ruler"] [HH.text st]
                         , HH.h1_                     [HH.text ai'.title]
                         ]
            <>
            [ maybe'
                (\_ -> HH.p_ [HH.text "ERROR: No speakerqueue found!"])
                (\(SpeakerQueue sq) ->
                  HH.div_
                  [ HH.p_
                    [ HH.strong_ [HH.text "Speaking: "]
                    , HH.text (maybe "–" (visualizeSpeaker state.attendees) sq.speaking)
                    ]
                  , HH.ol_
                    (map
                      (\s -> HH.li_ [HH.text (visualizeSpeaker state.attendees s) ])
                      sq.speakers)
                  ]
                )
                (AG.topSQ ai)
            ])

      where
        currentAI = AG.getCurrentAI state.agenda

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval =
      case _ of
        GotNewState s next ->
          H.put s *> pure next

