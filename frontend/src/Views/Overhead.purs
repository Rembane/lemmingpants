module Views.Overhead where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Prelude (type (~>), Unit, Void, const, pure)

type State = Int
data Query a = Query a

title :: String
title = "Overhead"

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const 0
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.p_ [HH.text "Overhead! :D"]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval =
      case _ of
        Query next -> pure next

