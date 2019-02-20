module Components.Admin.ListAttendees where

import Prelude

import Data.Array as A
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Types.Attendee as AT

type State = { attendees :: AT.AttendeeDB }

render :: forall p i. State -> H.HTML p i
render state =
  HH.table_ (
    [ HH.tr_
      [ HH.th_ [HH.text "CID"]
      , HH.th_ [HH.text "Name"]
      , HH.th_ [HH.text "Nick"]
      , HH.th_ [HH.text "Numbers"]
      ]
    ] <>
    (A.fromFoldable
      (map
        (\(AT.Attendee {cid, name, nick, numbers}) ->
          HH.tr_
            [ HH.td_ [HH.text cid]
            , HH.td_ [HH.text name]
            , HH.td_ [HH.text (fromMaybe "" nick)]
            , HH.td_ [HH.text (intercalate ", " $ map show $ A.sort numbers)]
            ]
        )
        (AT.listAttendees state.attendees)
      )
    )
  )
