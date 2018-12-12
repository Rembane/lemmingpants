module Components.Overhead where

import Data.Array as A
import Data.Either (Either(Right, Left))
import Data.Lens (preview, view)
import Data.Maybe (maybe, maybe')
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Void, identity, map, pure, ($), (*>), (<<<), (<>), (==))
import Types.Agenda (Agenda, AgendaItem(AgendaItem))
import Types.Agenda as AG
import Types.Attendee (AttendeeDB)
import Types.Speaker as S
import Types.SpeakerQueue (_Speakers, _Speaking)

type State =
  { agenda    :: Agenda
  , attendees :: AttendeeDB
  }
type Input = State

data Query a = GotNewState Input a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: HE.input GotNewState
    }
  where
    render :: State -> H.ComponentHTML Query
    render {agenda, attendees} =
      case AG.getCurrentAI agenda of
        Left e ->
          HH.div_ [HH.text e]
        Right ai@(AgendaItem {supertitle, title}) ->
          HH.div_
            ((maybe
               [HH.h1 [HP.id_ "with-ruler"] [HH.text title]]
               (\t ->  [HH.h1_ [HH.text t], HH.h2 [HP.id_ "with-ruler"] [HH.text title]])
               supertitle)
            <>
            [ maybe'
                (\_ -> HH.p_ [HH.text "ERROR: No speakerqueue found!"])
                (\sq ->
                  HH.div_
                  [ HH.p_
                    [ HH.strong_ [HH.text "Speaking: "]
                    , HH.text (maybe "–" (S.visualizeSpeaker attendees) (preview (_Speakers <<< _Speaking) sq))
                    ]
                  , HH.ol_
                    (A.fromFoldable (map
                      (\s -> HH.li_ [HH.text (S.visualizeSpeaker attendees s) ])
                      (A.dropWhile (\(S.Speaker {state}) -> state == S.Active) $ view _Speakers sq)))
                  ]
                )
                (AG.topSQ ai)
            ])

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval =
      case _ of
        GotNewState s next ->
          H.put s *> pure next

