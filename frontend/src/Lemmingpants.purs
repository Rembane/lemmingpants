module Lemmingpants where

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.Proxy (ProxyQ, proxy)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, const, pure, unit, (*>), (<$), (>>>))
import Routing.Hash (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Views.Admin as VA
import Views.Home as VH
import Views.Overhead as VO
import Views.Terminal as VT

data Location
  = Home
  | Terminal
  | Overhead
  | Admin

locations :: Match Location
locations
  =   (Terminal <$ lit "terminal")
  <|> (Overhead <$ lit "overhead")
  <|> (Admin    <$ lit "admin")
  <|> (Home     <$ lit "")

routeSignal
  :: forall o e
   . H.HalogenIO Query o (Aff (HA.HalogenEffects e))
  -> Eff (HA.HalogenEffects e) (Eff (HA.HalogenEffects e) Unit)
routeSignal driver
  = matches locations (\_ -> ChangePage >>> H.action >>> driver.query >>> launchAff_)

type State = { currentLocation :: Location }

data Query a
  = ChangePage Location a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const { currentLocation: Home }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query (ProxyQ (Const Void) Unit Void) Int m
    render state =
      HH.div_
        [ HH.ul_
          [ HH.li_ [HH.a [HP.href "#terminal"] [HH.text "Terminal"]]
          , HH.li_ [HH.a [HP.href "#overhead"] [HH.text "Overhead"]]
          , HH.li_ [HH.a [HP.href "#admin"]    [HH.text "Admin"]]
          , HH.li_ [HH.a [HP.href "#/"]        [HH.text "Home"]]
          ]
        , locationToSlot state.currentLocation
        ]
      where
        locationToSlot
          :: Location
          -> H.ParentHTML Query (ProxyQ (Const Void) Unit Void) Int m
        locationToSlot =
          case _ of
            Terminal -> go 1 VT.component
            Overhead -> go 2 VO.component
            Admin    -> go 3 VA.component
            Home     -> go 4 VH.component
          where
            go :: forall g
             . Int
            -> H.Component HH.HTML g Unit Void m
            -> H.ParentHTML Query (ProxyQ (Const Void) Unit Void) Int m
            go i c = HH.slot i (proxy c) unit (const Nothing)

    eval :: Query ~> H.ParentDSL State Query (ProxyQ (Const Void) Unit Void) Int Void m
    eval =
      case _ of
        ChangePage l next -> H.modify (_ {currentLocation = l}) *> pure next
