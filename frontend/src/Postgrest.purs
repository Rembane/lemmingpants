module Postgrest
  ( createURL
  , createWSURL
  , post
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
import Prelude (pure, (<#>), (<>), (>>=), (>>>))
import Simple.JSON (class WriteForeign, read, writeJSON)
import Types.Token (Token(..), parseToken)

foreign import urlPrefix   :: String
foreign import wsUrlPrefix :: String

createURL :: String -> String
createURL = (urlPrefix <> _)

createWSURL :: String -> String
createWSURL = (wsUrlPrefix <> _)

pgrequest
  :: forall d
   . WriteForeign d
   => String
   -> d
   -> AX.AffjaxRequest String
pgrequest url d = AX.defaultRequest
  { url     = url
  , headers =
      AX.defaultRequest.headers <> [ ContentType (MediaType "application/json") ]
  , content = Just (writeJSON d)
  }

post
  :: forall a d e
   . Respondable a
  => WriteForeign d
  => String
  -> d
  -> Aff (ajax :: AJAX | e) (AX.AffjaxResponse a)
post url d = AX.affjax ((pgrequest url d) { method = Left POST })

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
  let pg' = pgrequest url d
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
  post (createURL "/rpc/get_token") ""
  <#> \{response} ->
    pt response
    >>= (A.head
       >>> note (pure (ForeignError "No first item in array found.")))
    >>= \t -> parseToken t.token
  where
    pt :: Foreign -> Either MultipleErrors (Array { token :: String })
    pt = read
