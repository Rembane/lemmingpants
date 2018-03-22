module Lemmingpants where

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), Unit, Void, const, pure, show, unit, (*>), (<$), (>>>))
import Routing.Hash (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Views.Admin as VA
import Views.Home as VH
import Views.Login as VL
import Views.Overhead as VO
import Views.Terminal as VT

type ChildQuery = Coproduct5 VT.Query VO.Query VA.Query VL.Query VH.Query
type ChildSlot  = Either5 Int Int Int Int Int

data Location
  = Home
  | Terminal
  | Overhead
  | Admin
  | Login

locations :: Match Location
locations
  =   (Terminal <$ lit "terminal")
  <|> (Overhead <$ lit "overhead")
  <|> (Admin    <$ lit "admin")
  <|> (Login    <$ lit "login")
  <|> (Home     <$ lit "")

routeSignal
  :: forall o e
   . H.HalogenIO Query o (Aff (HA.HalogenEffects e))
  -> Eff (HA.HalogenEffects e) (Eff (HA.HalogenEffects e) Unit)
routeSignal driver
  = matches locations (\_ -> ChangePage >>> H.action >>> driver.query >>> launchAff_)

type State =
  { currentLocation :: Location
  , token           :: Maybe String
  }

data Query a
  = ChangePage Location   a
  | LoginMsg   VL.Message a

type LemmingPantsMonad e = Aff (HA.HalogenEffects (ajax :: AJAX | e))

component :: forall e. H.Component HH.HTML Query Unit Void (LemmingPantsMonad e)
component =
  H.parentComponent
    { initialState: const { currentLocation: Home, token: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (LemmingPantsMonad e)
    render state =
      HH.div_
        [ HH.ul_
          [ HH.li_ [HH.a [HP.href "#terminal"] [HH.text "Terminal"]]
          , HH.li_ [HH.a [HP.href "#overhead"] [HH.text "Overhead"]]
          , HH.li_ [HH.a [HP.href "#admin"]    [HH.text "Admin"]]
          , HH.li_ [HH.a [HP.href "#login"]    [HH.text "Login"]]
          , HH.li_ [HH.a [HP.href "#/"]        [HH.text "Home"]]
          ]
        , HH.p_
          [ HH.text "Our current token:"
          , HH.br_
          , HH.text (show state.token)
          ]
        , locationToSlot state.currentLocation
        ]
      where
        locationToSlot
          :: Location
          -> H.ParentHTML Query ChildQuery ChildSlot (LemmingPantsMonad e)
        locationToSlot =
          case _ of
            Terminal -> go CP.cp1 1 VT.component (const Nothing)
            Overhead -> go CP.cp2 2 VO.component (const Nothing)
            Admin    -> go CP.cp3 3 VA.component (const Nothing)
            Login    -> go CP.cp4 4 VL.component (HE.input LoginMsg)
            Home     -> go CP.cp5 5 VH.component (const Nothing)
          where
            go :: forall g o
             . CP.ChildPath g ChildQuery Int ChildSlot
            -> Int
            -> H.Component HH.HTML g Unit o (LemmingPantsMonad e)
            -> (o -> Maybe (Query Unit))
            -> H.ParentHTML Query ChildQuery ChildSlot (LemmingPantsMonad e)
            go cp i c f = HH.slot' cp i c unit f

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (LemmingPantsMonad e)
    eval =
      case _ of
        ChangePage l next -> H.modify (_ {currentLocation = l}) *> pure next
        LoginMsg   m next ->
          case m of
            VL.NewToken t -> H.modify (_ {token = Just t}) *> pure next
