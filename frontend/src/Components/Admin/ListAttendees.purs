module Components.Admin.ListAttendees where

import Prelude

import Data.Array as A
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Types.Attendee as AT

type State = { attendees :: AT.AttendeeDB }
data Query a = Query a

title :: String
title = "List attendees"

component :: forall m. H.Component HH.HTML Query State Void m
component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
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

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval =
      case _ of
        Query next -> pure next

