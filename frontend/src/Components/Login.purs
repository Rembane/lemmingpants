module Components.Login where

import Affjax as AX
import Affjax.ResponseFormat as Res
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (foldMap)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import FormHelpers (FieldError, fieldScaffolding, isNonEmpty)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, const, discard, identity, pure, unit, ($), (*>), (<$>), (<<<), (>>=))
import Simple.JSON (readJSON)
import Types.Flash as FL
import Types.Token (Token, parseToken)

type State = { token :: Token }

data Query a
  = Formless (F.Message' Form) a

type ChildQuery = F.Query' Form Aff

data Message
  = NewToken Token
  | Flash    FL.Flash

newtype Form r f = Form (r
  ( username  :: f FieldError String String
  , password  :: f FieldError String String
  ))
derive instance newtypeForm :: Newtype (Form r f) _

component :: H.Component HH.HTML Query State Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery Unit Aff
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Login"]
        , HH.slot
            unit
            F.component
              { initialInputs, validators, render: renderFormless }
              (HE.input Formless)
        ]
      where
        proxy = F.FormProxy :: F.FormProxy Form

        initialInputs :: Form Record F.InputField
        initialInputs = F.mkInputFields proxy

        validators :: Form Record (F.Validation Form Aff)
        validators = Form
          { username: isNonEmpty
          , password: isNonEmpty
          }

        renderFormless :: F.State Form Aff -> F.HTML' Form Aff
        renderFormless s =
          HH.form
          [ HE.onSubmit (HE.input_ F.submit) ]
            [ fieldScaffolding "Username"
              [ HH.input
                [ HP.value $ F.getInput r.username s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.username
                , HP.type_ HP.InputText
                , HP.required true
                ]
              ]
            , fieldScaffolding "Password"
              [ HH.input
                [ HP.value $ F.getInput r.password s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.password
                , HP.type_ HP.InputPassword
                , HP.required true
                ]
              ]
            , HH.p_
              [ HH.input
                [ HP.type_ HP.InputSubmit
                , HP.value "Login!"
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
              -- You can use signedInAjax even if you're not signed in.
              -- It just needs a token, and everybody gets a token upon
              -- loading the page so they can connect to the websocket
              -- server.
              {token} <- H.get
              (result <$> H.liftAff (PG.signedInAjax (createURL "/rpc/login") token POST Res.string [] { username: form.username, password: form.password })) >>=
              case _ of
                Left  es -> H.raise $ Flash $ FL.mkFlash (foldMap renderForeignError es) FL.Error
                Right t  -> (H.raise $ NewToken t)
                           *> (H.raise $ Flash $ FL.mkFlash "You are now logged in!" FL.Info)
              pure next
            _ -> pure next

      where
        pt :: String -> Either MultipleErrors (Array { token :: String })
        pt = readJSON

        result :: AX.Response (Either AX.ResponseFormatError String) -> Either MultipleErrors Token
        result {body} =
          lmap (pure <<< ForeignError <<< Res.printResponseFormatError) body
            >>= pt
            >>= note (pure $ ForeignError "Expected array with length >0, got something else... from: /rpc/login") <<< A.head
            >>= \t -> parseToken t.token
