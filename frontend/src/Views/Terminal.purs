module Views.Terminal where

import Control.Monad.Aff (Aff)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.MediaType (MediaType(..))
import Data.StrMap (StrMap, empty, insert, lookup)
import Debug.Trace (traceA, traceAnyA)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Prelude (type (~>), Unit, Void, bind, discard, pure, (*>), (<$>), (<>))
import Simple.JSON (writeJSON)
import Utils (withLabel)

type State =
  { formValues :: StrMap String
  , token      :: Maybe String
  }

data Query a
  = UpdateField String String a
  | SubmitForm a
  | HandleInput (Maybe String) a

type Input = Maybe String -- The token, if there is one.

data Message = Flash String

-- TODO: Form library!

component :: forall e. H.Component HH.HTML Query Input Message (Aff (LemmingPantsEffects e))
component =
  H.component
    { initialState: \i -> { formValues: empty, token: i }
    , render
    , eval
    , receiver
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Terminal"]
        , case state.token of
            Nothing -> HH.p_ [ HH.text "You need to be logged in. Please login!" ]
            Just _  ->
              HH.form
              [ HE.onSubmit (HE.input_ SubmitForm) ]
              [ withLabel UpdateField HP.InputNumber true  "id"   "Number"
              , withLabel UpdateField HP.InputText   true  "cid"  "CID"
              , withLabel UpdateField HP.InputText   true  "name" "Full name"
              , withLabel UpdateField HP.InputText   false "nick" "Nickname, if any"
              , HH.p_
                [ HH.input
                  [ HP.type_ HP.InputSubmit
                  , HP.value "I am attending this meeting!"
                  ]
                ]
              ]
        ]

    eval :: Query ~> H.HalogenM State Query (Const Void) Void Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        UpdateField k v next ->
          H.modify (\s -> s { formValues = insert k v s.formValues })
          *> pure next
        SubmitForm next      -> do
          token <- H.gets (\s -> s.token)
          case token of
            Nothing -> H.raise (Flash "You are not logged in. Please login.")
            Just t  -> do
              d <- writeJSON <$> H.gets (\s -> s.formValues)
              let req = AX.defaultRequest
              r <- H.liftAff (AX.affjax (
                     req { url     = "http://localhost:3000/attendee"
                         , headers =
                             req.headers <>
                               [ ContentType (MediaType "application/json")
                               , RequestHeader "Authorization" ("Bearer " <> t)
                               ]
                         , method  = Left POST
                         , content = Just d
                         }))
              -- The Location-header contains where the new Attendee URL.
              case r.status of
                StatusCode 201 -> do -- The `Created` HTTP status code.
                  name <- H.gets (\s -> unsafePartial (fromJust (lookup "name" s.formValues)))
                  H.modify (\s -> s { formValues = empty :: StrMap String })
                  H.raise (Flash ("Thank you for registering, " <> name))
                _ ->
                  traceAnyA r *> traceA r.response
          pure next
        HandleInput mt next  ->
          H.modify (\s -> s { token = mt })
          *> pure next

    receiver :: Input -> Maybe (Query Unit)
    receiver = HE.input HandleInput
