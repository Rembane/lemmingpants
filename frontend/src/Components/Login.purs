module Components.Login where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, ForeignError(..), MultipleErrors, renderForeignError)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing))
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, const, discard, id, pure, unit, ($), (*>), (<<<), (>>=))
import Simple.JSON (read)
import Types.Token (Token, parseToken)

type State = { token :: Token }

data Query a
  = FormMsg F.Message a

data Message
  = NewToken Token
  | Flash    String


component :: forall e. H.Component HH.HTML Query State Message (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query F.Query Unit (Aff (LemmingPantsEffects e))
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Login"]
        , HH.slot
            unit
            (F.component "Login!"
              [ mkField "username" "Username" [HP.type_ HP.InputText,     HP.required true]
              , mkField "password" "Password" [HP.type_ HP.InputPassword, HP.required true]
              ]
            )
            unit
            (HE.input FormMsg)
        ]

    eval :: Query ~> H.HalogenM State Query F.Query Unit Message (Aff (LemmingPantsEffects e))
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
              r <- H.liftAff $ PG.signedInAjax (createURL "/rpc/login") token POST [] m'
              case result r of
                Left  es -> H.raise $ Flash $ foldMap renderForeignError es
                Right t  -> (H.raise $ NewToken t)
                           *> (H.raise $ Flash "You are now logged in!")
              pure next

      where
        pt :: Foreign -> Either MultipleErrors (Array { token :: String })
        pt = read

        result :: AX.AffjaxResponse Foreign -> Either MultipleErrors Token
        result r =
          pt r.response
            >>= note (pure $ ForeignError "Expected array with length >0, got something else... from: /rpc/login") <<< A.head
            >>= \t -> parseToken t.token
