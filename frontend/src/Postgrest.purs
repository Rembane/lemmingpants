module Postgrest
  ( createURL
  , createWSURL
  , signedInAjax
  , useToken
  , emptyResponse
  , requestAnonymousToken
  ) where

-- | Here we keep the functions to make our interactions with Postgrest a breeze.

import Data.Array as A
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Effect.Aff (Aff)
import Foreign (ForeignError(..), MultipleErrors)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request as Req
import Network.HTTP.Affjax.Response as Res
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (pure, ($), (<#>), (<>), (>>=), (>>>))
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Types.Token (Token(..), parseToken)

foreign import urlPrefix   :: String
foreign import wsUrlPrefix :: String

createURL :: String -> String
createURL = (<>) urlPrefix

createWSURL :: String -> String
createWSURL = (<>) wsUrlPrefix

pgrequest
  :: URL
  -> AX.AffjaxRequest
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
  :: forall a i
  .  WriteForeign i
  => URL
  -> Token
  -> Method
  -> Res.Response a
  -> Array RequestHeader
  -> i
  -> Aff (AX.AffjaxResponse a)
signedInAjax url token method r hs d =
  let pg' = (pgrequest url) { content = Just (Req.string $ writeJSON d) }
   in AX.affjax r (pg' { headers = pg'.headers <> [useToken token] <> hs, method = Left method })

emptyResponse
  :: forall i
   . WriteForeign i
  => URL
  -> Token
  -> Method
  -> i
  -> Aff (AX.AffjaxResponse String)
emptyResponse url token m d = signedInAjax url token m (Res.string) mempty d

-- | Request a new, anonymous token
requestAnonymousToken :: Aff (Either MultipleErrors Token)
requestAnonymousToken =
  AX.affjax Res.string ((pgrequest (createURL "/rpc/get_token")) { method = Left POST })
  <#> \{response} ->
    pt response
    >>= (A.head
       >>> note (pure (ForeignError "No first item in array found.")))
    >>= \t -> parseToken t.token
  where
    pt :: String -> Either MultipleErrors (Array { token :: String })
    pt = readJSON
