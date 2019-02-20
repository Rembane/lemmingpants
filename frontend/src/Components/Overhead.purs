module Components.Overhead where

import Data.Array as A
import Data.Either (Either(Right, Left))
import Data.Lens (preview, view)
import Data.Maybe (maybe, maybe')
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (map, ($), (<<<), (<>), (==))
import Types.Agenda (Agenda, AgendaItem(AgendaItem))
import Types.Agenda as AG
import Types.Attendee (AttendeeDB)
import Types.Speaker as S
import Types.SpeakerQueue (_Speakers, _Speaking)

type State =
  { agenda    :: Agenda
  , attendees :: AttendeeDB
  }

render :: forall p i. State -> H.HTML p i
render {agenda, attendees} =
  case AG.getCurrentAI agenda of
    Left e ->
      HH.div_ [HH.text e]
    Right ai@(AgendaItem {parent, title}) ->
      HH.div_
        ((maybe
           [HH.h1 [HP.id_ "with-ruler"] [HH.text title]]
           (\(AgendaItem pai) -> [HH.h1_ [HH.text pai.title], HH.h2 [HP.id_ "with-ruler"] [HH.text title]])
           parent)
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

