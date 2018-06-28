module Components.Registration where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Monoid (mempty)
import Data.String (null)
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Foreign (MultipleErrors)
import Foreign.Object (delete, lookup)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax.Response (string)
import Network.HTTP.StatusCode (StatusCode(..))
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, discard, identity, pure, unit, ($), (*>), (<$>), (<>))
import Simple.JSON (readJSON)
import Types.Flash as FL
import Types.Token (Token)

type State =
  { token    :: Token
  , showForm :: Boolean
  }
type Input = State

data Query a
  = HandleInput Input a
  | FormMsg F.Message a

data Message
  = Flash FL.Flash
  | FrmVsbl Boolean

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver
    }
  where
    render :: State -> H.ParentHTML Query F.Query Unit Aff
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Registration"]
        , if state.showForm then form else HH.text "" ]
      where
        form =
          HH.slot
            unit
            (F.component "I am attending this meeting!"
              [ mkField "id"   "Number *"         [HP.type_ HP.InputNumber, HP.required true]
              , mkField "cid"  "CID *"            [HP.type_ HP.InputText,   HP.required true]
              , mkField "name" "Full name *"      [HP.type_ HP.InputText,   HP.required true]
              , mkField "nick" "Nickname, if any" [HP.type_ HP.InputText,   HP.required false]
              ]
            )
            unit
            (HE.input FormMsg)

    eval :: Query ~> H.HalogenM State Query F.Query Unit Message Aff
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
              r     <- H.liftAff (PG.signedInAjax (createURL "/rpc/create_attendee") token POST string mempty m'')
              case r.status of
                StatusCode 200 -> do -- The `Created` HTTP status code.
                  H.raise (FrmVsbl false)
                  *> (H.raise $ Flash $ FL.mkFlash ("Thank you for registering, " <> fromMaybe "ERROR! EXTERMINATE!" (lookup "name" m')) FL.Info)
                StatusCode 409 -> do -- The `Conflict` HTTP status code.
                  -- Do we already have a person with the same number?
                  case parseError r.response of
                    Left e   -> traceM e
                    Right r' -> H.raise $ Flash $ FL.mkFlash ("ERROR: " <> r'.details) FL.Error
                _ ->
                  traceM r *> traceM r.response
              pure next

        HandleInput mt next ->
          H.put mt *> pure next

      where
        parseError :: String -> Either MultipleErrors {hint :: Maybe String, details :: String, code :: String, message :: String}
        parseError = readJSON

    receiver :: Input -> Maybe (Query Unit)
    receiver = HE.input HandleInput
