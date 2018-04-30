module Postgrest
  ( createURL
  , createWSURL
  , signedInAjax
  , useToken
  , emptyResponse
  , requestAnonymousToken
  ) where

-- | Here we keep the functions to make our interactions with Postgrest a breeze.

import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Foreign (Foreign, ForeignError(..), MultipleErrors)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (Unit, pure, (<#>), (<>), (>>=), (>>>))
import Simple.JSON (class WriteForeign, read, writeJSON)
import Types.Token (Token(..), parseToken)

foreign import urlPrefix   :: String
foreign import wsUrlPrefix :: String

createURL :: String -> String
createURL = (urlPrefix <> _)

createWSURL :: String -> String
createWSURL = (wsUrlPrefix <> _)

pgrequest
  :: String
  -> AX.AffjaxRequest Unit
pgrequest url =
  AX.defaultRequest
    { url     = url
    , headers =
        AX.defaultRequest.headers <> [ ContentType (MediaType "application/json") ]
    }

useToken
  :: Token
  -> RequestHeader
useToken (Token t) = RequestHeader "Authorization" ("Bearer " <> t.raw)

signedInAjax
  :: forall a d e
   . Respondable a
  => WriteForeign d
  => String
  -> Token
  -> Method
  -> Array RequestHeader
  -> d
  -> Aff (ajax :: AJAX | e) (AX.AffjaxResponse a)
signedInAjax url token m hs d =
  let pg' = (pgrequest url) { content = Just (writeJSON d) }
   in AX.affjax (pg' { headers = pg'.headers <> [useToken token] <> hs, method = Left m })

emptyResponse
  :: forall d e
   . WriteForeign d
  => String
  -> Token
  -> Method
  -> d
  -> Aff (ajax :: AJAX | e) (AX.AffjaxResponse String)
emptyResponse url token m d = signedInAjax url token m mempty d

-- | Request a new, anonymous token
requestAnonymousToken :: forall e. Aff (ajax :: AJAX | e) (Either MultipleErrors Token)
requestAnonymousToken =
  AX.affjax (pgrequest (createURL "/rpc/get_token")) { method = Left POST }
  <#> \{response} ->
    pt response
    >>= (A.head
       >>> note (pure (ForeignError "No first item in array found.")))
    >>= \t -> parseToken t.token
  where
    pt :: Foreign -> Either MultipleErrors (Array { token :: String })
    pt = read
