module Types.Token
  ( Payload(..)
  , Token(..)
  , parseToken
  , saveToken
  , loadToken
  ) where

import Browser.WebStorage (WEB_STORAGE, getItem, localStorage, setItem)
import Control.Error.Util ((!?))
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except.Trans (except, runExceptT)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foreign (ForeignError(ForeignError), MultipleErrors)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Newtype (class Newtype)
import Data.Record.ShowRecord (showRecord)
import Data.String (Pattern(..), split)
import Prelude (class Eq, class Show, Unit, pure, show, ($), (<#>), (<<<), (<>), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readJSON)

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
  show (Payload p) = showRecord p

newtype Token = Token
  { raw     :: String
  , payload :: Payload
  }

instance showToken :: Show Token where
  show (Token t) = "Token: " <> showRecord t

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
  >>=
    lmap (pure <<< ForeignError <<< show) <<< atob
  >>=
    readJSON
  <#>
    \payload -> Token {raw, payload}

saveToken :: forall e. Token -> Aff (webStorage :: WEB_STORAGE | e) Unit
saveToken (Token t) = liftEff' (setItem localStorage tokenKey t.raw)

loadToken :: forall e. Aff (webStorage :: WEB_STORAGE | e) (Either MultipleErrors Token)
loadToken = liftEff' $ runExceptT $
  getItem localStorage tokenKey
    !? (pure (ForeignError "No token found."))
    >>= parseToken
    >>> except

