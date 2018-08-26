module Types.Token
  ( Payload(..)
  , Token(..)
  , parseToken
  , saveToken
  , loadToken
  , removeToken
  ) where

import Effect.Class

import Control.Error.Util ((!?))
import Control.Monad.Except.Trans (except, runExceptT)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Foreign (ForeignError(..), MultipleErrors)
import Prelude (class Eq, class Show, Unit, pure, show, ($), (<#>), (<<<), (<>), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readJSON)
import Web.HTML (Window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- | This module handles tokens.

-- | The key for the token
tokenKey :: String
tokenKey = "token"

newtype Payload = Payload
  { role :: String
  , exp  :: Int
  , mode :: String
  }
derive instance         ntPl :: Newtype     Payload _
derive newtype instance rfPl :: ReadForeign Payload
derive instance         eqPl :: Eq          Payload

instance showPl :: Show Payload where
  show (Payload p) = show p

newtype Token = Token
  { raw     :: String
  , payload :: Payload
  }

instance showToken :: Show Token where
  show (Token t) = "Token: " <> show t

foreign import _atob
  :: Fn3
       (forall x y. x -> Either x y)
       (forall x y. y -> Either x y)
       String
       (Either Error String)

atob :: String -> (Either Error String)
atob s = runFn3 _atob Left Right s

parseToken :: String -> Either MultipleErrors Token
parseToken raw =
  note
    (pure (ForeignError "Token has weird format. It lacks dots. I cannot parse this."))
    (A.index (Pattern "." `split` raw) 1)
  >>= lmap (pure <<< ForeignError <<< show) <<< atob
  >>= readJSON
  <#> \payload -> Token {raw, payload}

saveToken :: Window -> Token -> Aff Unit
saveToken w (Token t) = liftEffect (localStorage w >>= setItem tokenKey t.raw)

loadToken :: Window -> Aff (Either MultipleErrors Token)
loadToken w = liftEffect $ localStorage w >>=
  \s -> runExceptT $ getItem tokenKey s
    !? (pure (ForeignError "No token found."))
    >>= parseToken
    >>> except

removeToken :: Window -> Aff Unit
removeToken w = liftEffect $ localStorage w >>= removeItem tokenKey
