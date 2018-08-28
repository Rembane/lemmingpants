module Components.Login where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (foldMap)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing))
import Effect.Aff (Aff)
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as Res
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, const, discard, identity, pure, unit, ($), (*>), (<<<), (>>=))
import Simple.JSON (readJSON)
import Types.Flash as FL
import Types.Token (Token, parseToken)

type State = { token :: Token }

data Query a
  = FormMsg F.Message a

data Message
  = NewToken Token
  | Flash    FL.Flash


component :: H.Component HH.HTML Query State Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query F.Query Unit Aff
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Login"]
        , HH.slot
            unit
            (F.component "Login!"
              [ mkField "username" "Username *" [HP.type_ HP.InputText,     HP.required true]
              , mkField "password" "Password *" [HP.type_ HP.InputPassword, HP.required true]
              ]
            )
            unit
            (HE.input FormMsg)
        ]

    eval :: Query ~> H.HalogenM State Query F.Query Unit Message Aff
    eval =
      case _ of
        FormMsg m next ->
          case m of
            F.FormSubmitted m' -> do
              -- You can use signedInAjax even if you're not signed in.
              -- It just needs a token, and everybody gets a token upon
              -- loading the page so they can connect to the websocket
              -- server.
              token <- H.gets (\s -> s.token)
              r <- H.liftAff $ PG.signedInAjax (createURL "/rpc/login") token POST Res.string [] m'
              case result r of
                Left  es -> H.raise $ Flash $ FL.mkFlash (foldMap renderForeignError es) FL.Error
                Right t  -> (H.raise $ NewToken t)
                           *> (H.raise $ Flash $ FL.mkFlash "You are now logged in!" FL.Info)
              pure next

      where
        pt :: String -> Either MultipleErrors (Array { token :: String })
        pt = readJSON

        result :: AX.Response (Either AX.ResponseFormatError String) -> Either MultipleErrors Token
        result {body} =
          lmap (pure <<< ForeignError <<< Res.printResponseFormatError) body
            >>= pt
            >>= note (pure $ ForeignError "Expected array with length >0, got something else... from: /rpc/login") <<< A.head
            >>= \t -> parseToken t.token
