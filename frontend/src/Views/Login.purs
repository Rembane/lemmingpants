module Views.Login where

import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Data.StrMap (StrMap, insert)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Partial.Unsafe (unsafePartial)
import Prelude (type (~>), Unit, bind, const, discard, pure, (*>), (<$>))
import Simple.JSON (read, writeJSON)
import Utils (withLabel)

type State = { formState :: StrMap String }
data Query a
  = UpdateField String String a
  | SubmitForm a

data Message
  = NewToken String
  | Flash    String

component :: forall e. H.Component HH.HTML Query Unit Message (Aff (LemmingPantsEffects e))
component =
  H.component
    { initialState: const { formState: mempty }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Login"]
        , HH.form
          [ HE.onSubmit (HE.input_ SubmitForm) ]
          [ withLabel UpdateField HP.InputText     true  "username" "Username"
          , withLabel UpdateField HP.InputPassword true  "password" "Password"
          , HH.p_
            [ HH.input
              [ HP.type_ HP.InputSubmit
              , HP.value "Login!"
              ]
            ]
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        UpdateField k v next ->
          H.modify (\s -> s { formState = insert k v s.formState })
          *> pure next
        SubmitForm next      -> do
          d <- writeJSON <$> H.gets (\s -> s.formState)
          let req = AX.defaultRequest
          r <- H.liftAff (AX.affjax (
                 req { url     = "http://localhost:3000/rpc/login"
                     , headers = req.headers `A.snoc` (ContentType (MediaType "application/json"))
                     , method  = Left POST
                     , content = Just d
                 }))
          case parseToken r.response of
            Left  es -> H.raise (Flash (foldMap renderForeignError es))
            Right ts -> let t = unsafePartial (fromJust (A.head ts))
                         in H.raise (NewToken t.token )
          pure next

      where
        parseToken :: Foreign -> Either MultipleErrors (Array { token :: String })
        parseToken = read
