module Lemmingpants where

import Browser.WebStorage (localStorage, setItem)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..))
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, const, pure, unit, (*>), (<$), (<>), (>>>))
import Routing.Hash (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Components.Admin as CA
import Components.Home as CH
import Components.Login as CL
import Components.Overhead as CO
import Components.Terminal as CT

type ChildQuery = Coproduct5 CT.Query CO.Query CA.Query CL.Query CH.Query
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
  , flash           :: Maybe String
  }

data Query a
  = ChangePage  Location   a
  | AdminMsg    CA.Message a
  | LoginMsg    CL.Message a
  | TerminalMsg CT.Message a

tokenKey :: String
tokenKey = "token"

-- | Takes a token as first argument.
component :: forall e. Maybe String -> H.Component HH.HTML Query Unit Void (Aff (LemmingPantsEffects e))
component token =
  H.parentComponent
    { initialState: const { currentLocation: Home, token: token, flash: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }
  where

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (LemmingPantsEffects e))
    render state =
      HH.div_
        (( case state.flash of
            Nothing -> []
            Just  s -> [ HH.div_ [ HH.text s ] ]
        ) <>
        [ HH.ul_
          [ HH.li_ [HH.a [HP.href "#terminal"] [HH.text "Terminal"]]
          , HH.li_ [HH.a [HP.href "#overhead"] [HH.text "Overhead"]]
          , HH.li_ [HH.a [HP.href "#admin"]    [HH.text "Admin"]]
          , HH.li_ [HH.a [HP.href "#login"]    [HH.text "Login"]]
          , HH.li_ [HH.a [HP.href "#/"]        [HH.text "Home"]]
          ]
        , locationToSlot state.currentLocation state
        ])
      where
        locationToSlot
          :: Location
          -> State
          -> H.ParentHTML Query ChildQuery ChildSlot (Aff (LemmingPantsEffects e))
        locationToSlot l s =
          case l of
            Terminal -> go CP.cp1 1 CT.component s.token (HE.input TerminalMsg)
            Overhead -> go CP.cp2 2 CO.component unit    (const Nothing)
            Admin    -> go CP.cp3 3 CA.component s.token (HE.input AdminMsg)
            Login    -> go CP.cp4 4 CL.component unit    (HE.input LoginMsg)
            Home     -> go CP.cp5 5 CH.component unit    (const Nothing)
          where
            go :: forall g i o
             . CP.ChildPath g ChildQuery Int ChildSlot
            -> Int
            -> H.Component HH.HTML g i o (Aff (LemmingPantsEffects e))
            -> i
            -> (o -> Maybe (Query Unit))
            -> H.ParentHTML Query ChildQuery ChildSlot (Aff (LemmingPantsEffects e))
            go cp p c i f = HH.slot' cp p c i f

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        ChangePage l next -> H.modify (_ {currentLocation = l}) *> pure next
        AdminMsg   m next ->
          case m of
            CA.Flash s -> flash s next
        LoginMsg   m next ->
          case m of
            CL.NewToken t ->
              H.modify (_ {token = Just t})
              *> H.liftEff (setItem localStorage tokenKey t)
              *> pure next
            CL.Flash s -> flash s next
        TerminalMsg m next ->
          case m of
            CT.Flash s -> flash s next
      where
        flash s next =
          H.modify (_ {flash = Just s})
          *> pure next
