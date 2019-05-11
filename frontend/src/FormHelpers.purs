module FormHelpers where

import Prelude

import Data.Either (Either(..), note)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList)
import Data.String (null)
import Effect (Effect)
import Foreign (ForeignError)
import Formless as F
import Halogen.HTML as HH

foreign import setFocusImpl :: Fn1 String (Effect Unit)

setFocus :: String -> Effect Unit
setFocus = runFn1 setFocusImpl

data FieldError
  = Required
  | NotAnInt
  | ParseJSONError (NonEmptyList ForeignError)

-- | The structure around a form field.
fieldScaffolding :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
fieldScaffolding label field =
  HH.p_
    [ HH.label_
      ([ HH.text label ] <> field)
    ]

isNonEmpty :: forall form m. Monad m => F.Validation form m FieldError String String
isNonEmpty = F.hoistFnE_ $ \str ->
                if null str
                  then Left Required
                  else Right str

isInt :: forall form m. Monad m => F.Validation form m FieldError String Int
isInt = F.hoistFnE_ (note NotAnInt <<< fromString)

