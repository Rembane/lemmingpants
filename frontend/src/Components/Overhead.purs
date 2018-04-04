module Components.Overhead where

import Types

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Prelude (type (~>), Void, const, id, pure)

type State =
  { pageTitle :: String
  }
type Input = State

data Query a = Query a

title :: String
title = "Overhead"

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: id
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

