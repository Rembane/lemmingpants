module Components.Registration where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.StrMap (delete, lookup)
import Data.String (null)
import Debug.Trace (traceA, traceAnyA)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.StatusCode (StatusCode(..))
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, discard, pure, unit, (*>), (<$>), (<>))

type State = { token :: Maybe String }

data Query a
  = HandleInput (Maybe String) a
  | FormMsg F.Message a

-- | The token, if there is one.
type Input = Maybe String

data Message = Flash String

component :: forall e. H.Component HH.HTML Query Input Message (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState: \i -> { token: i }
    , render
    , eval
    , receiver
    }
  where
    render :: State -> H.ParentHTML Query F.Query Unit (Aff (LemmingPantsEffects e))
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Registration"]
        , case state.token of
            Nothing -> HH.p_ [ HH.text "You need to be logged in. Please login!" ]
            Just _  ->
              HH.slot
                unit
                (F.component "I am attending this meeting!"
                  [ mkField "id"   "Number"           [HP.type_ HP.InputNumber, HP.required true]
                  , mkField "cid"  "CID"              [HP.type_ HP.InputText,   HP.required true]
                  , mkField "name" "Full name"        [HP.type_ HP.InputText,   HP.required true]
                  , mkField "nick" "Nickname, if any" [HP.type_ HP.InputText,   HP.required false]
                  ]
                )
                unit
                (HE.input FormMsg)
        ]

    eval :: Query ~> H.HalogenM State Query F.Query Unit Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        FormMsg m next -> do
            case m of
              F.FormSubmitted m' -> do
                -- If the nick field is empty, remove it so we don't get empty nicks in the database.
                let m'' = case null <$> lookup "nick" m' of
                            Just true -> delete "nick" m'
                            _         -> m'
                token <- H.gets (\s -> s.token)
                er    <- H.liftAff (PG.signedInAjax (createURL "/rpc/create_attendee") token POST mempty m'')
                case er of
                  Left es -> H.raise (Flash es)
                  Right r ->
                    case r.status of
                      -- The Location-header contains the new Attendee URL.
                      StatusCode 200 -> do -- The `Created` HTTP status code.
                        H.raise (Flash ("Thank you for registering, " <> fromMaybe "ERROR! EXTERMINATE!" (lookup "name" m')))
                      _ ->
                        traceAnyA r *> traceA r.response
                pure next
        HandleInput mt next ->
          H.modify (\s -> s { token = mt })
          *> pure next

    receiver :: Input -> Maybe (Query Unit)
    receiver = HE.input HandleInput
