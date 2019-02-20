module Components.Registration where

import Affjax.ResponseFormat as Res
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor
import Data.Either (Either, either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Foreign (ForeignError(..), MultipleErrors)
import FormHelpers (FieldError, fieldScaffolding, isInt, isNonEmpty)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, discard, identity, pure, unit, ($), (*>), (<<<), (<>), (=<<))
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
  | Formless (F.Message' Form) a

type ChildQuery = F.Query' Form Aff

data Message
  = Flash FL.Flash
  | FrmVsbl Boolean

newtype Form r f = Form (r
  ( id   :: f FieldError String Int
  , cid  :: f FieldError String String
  , name :: f FieldError String String
  , nick :: f FieldError String String
  ))
derive instance newtypeForm :: Newtype (Form r f) _

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery Unit Aff
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Registration"]
        , if state.showForm then form else HH.text "" ]
      where
        form =
          HH.slot
            unit
            F.component
              { initialInputs, validators, render: renderFormless }
              (HE.input Formless)

        proxy = F.FormProxy :: F.FormProxy Form

        initialInputs :: Form Record F.InputField
        initialInputs = F.mkInputFields proxy

        validators :: Form Record (F.Validation Form Aff)
        validators = Form
          { id: isInt <<< isNonEmpty
          , cid: isNonEmpty
          , name: isNonEmpty
          , nick: identity
          }

        renderFormless :: F.State Form Aff -> F.HTML' Form Aff
        renderFormless s =
          HH.form
            [ HE.onSubmit (HE.input_ F.submit) ]
            [ fieldScaffolding "Number"
              [ HH.input
                [ HP.value $ F.getInput r.id s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.id
                , HP.type_ HP.InputNumber
                , HP.required true
                ]
              ]
            , fieldScaffolding "CID"
              [ HH.input
                [ HP.value $ F.getInput r.cid s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.cid
                , HP.type_ HP.InputText
                , HP.required true
                ]
              ]
            , fieldScaffolding "Full name"
              [ HH.input
                [ HP.value $ F.getInput r.name s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.name
                , HP.type_ HP.InputText
                , HP.required true
                ]
              ]
            , fieldScaffolding "Nickname, if any"
              [ HH.input
                [ HP.value $ F.getInput r.nick s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.nick
                , HP.type_ HP.InputText
                , HP.required false
                ]
              ]
            , HH.p_
              [ HH.input
                [ HP.type_ HP.InputSubmit
                , HP.value "I am attending this meeting!"
                ]
              ]
            ]
          where
            r = F.mkSProxies proxy

    eval :: Query ~> H.HalogenM State Query ChildQuery Unit Message Aff
    eval =
      case _ of
        Formless m next -> do
          case m of
            F.Submitted formOutput -> do
              let form = F.unwrapOutputFields formOutput
              {token} <- H.get
              {status, body} <- H.liftAff (PG.signedInAjax (createURL "/rpc/create_attendee") token POST Res.string mempty form)
              case status of
                StatusCode 200 -> -- The `Created` HTTP status code.
                  H.raise (FrmVsbl false)
                  *> (H.raise $ Flash $ FL.mkFlash ("Thank you for registering, " <> form.name) FL.Info)
                StatusCode 409 -> do -- The `Conflict` HTTP status code.
                  -- Do we already have a person with the same number?
                  either
                    traceM
                    (\{details} -> H.raise $ Flash $ FL.mkFlash ("ERROR: " <> details) FL.Error)
                    (parseError =<< lmap (pure <<< ForeignError <<< Res.printResponseFormatError) body)
                _ ->
                  traceM status *> traceM body
              pure next
            _ -> pure next

        HandleInput mt next ->
          H.put mt *> pure next

      where
        parseError
          :: String
          -> Either
              MultipleErrors
              { hint    :: Maybe String
              , details :: String
              , code    :: String
              , message :: String
              }
        parseError = readJSON

