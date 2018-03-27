module Components.Login where

import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.Maybe (Maybe(..), fromJust)
import Effects (LemmingPantsEffects)
import Components.Forms as F
import Components.Forms.Field (mkField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, const, discard, pure, unit, (*>))
import Simple.JSON (read)

type State = Unit

data Query a
  = FormMsg F.Message a

data Message
  = NewToken String
  | Flash    String


component :: forall e. H.Component HH.HTML Query Unit Message (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState: const unit
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
              r <- H.liftAff (PG.post "http://localhost:3000/rpc/login" m')
              case parseToken r.response of
                Left  es -> H.raise (Flash (foldMap renderForeignError es))
                Right ts -> let t = unsafePartial (fromJust (A.head ts))
                             in H.raise (NewToken t.token)
                             *> H.raise (Flash "You are now logged in!")
              pure next

      where
        parseToken :: Foreign -> Either MultipleErrors (Array { token :: String })
        parseToken = read
