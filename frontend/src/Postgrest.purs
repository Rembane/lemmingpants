module Postgrest
  ( createURL
  , createWSURL
  , signedInAjax
  , useToken
  , emptyResponse
  , requestAnonymousToken
  ) where

-- | Here we keep the functions to make our interactions with Postgrest a breeze.

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.RequestBody as Req
import Affjax.ResponseFormat as Res
import Data.Array as A
import Data.Either (Either(..), either, note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Effect.Aff (Aff)
import Foreign (ForeignError(..), MultipleErrors)
import Prelude (Unit, pure, ($), (<#>), (<>), (>>=), (>>>), (<<<))
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Types.Token (Token(..), parseToken)

foreign import urlPrefix   :: String
foreign import wsUrlPrefix :: String

createURL :: String -> String
createURL = (<>) urlPrefix

createWSURL :: String -> String
createWSURL = (<>) wsUrlPrefix

pgrequest
  :: AX.URL
  -> AX.Request Unit
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
  => AX.URL
  -> Token
  -> Method
  -> Res.ResponseFormat a
  -> Array RequestHeader
  -> i
  -> Aff (AX.Response (Either AX.ResponseFormatError a))
signedInAjax url token method r hs d =
  let pg' = pgrequest url
   in AX.request (pg' { headers        = pg'.headers <> [useToken token] <> hs
                      , method         = Left method
                      , content        = Just $ Req.string $ writeJSON d
                      , responseFormat = r
                      })

emptyResponse
  :: forall i
   . WriteForeign i
  => AX.URL
  -> Token
  -> Method
  -> i
  -> Aff (AX.Response (Either AX.ResponseFormatError String))
emptyResponse url token m d = signedInAjax url token m Res.string mempty d

-- | Request a new, anonymous token
requestAnonymousToken :: Aff (Either MultipleErrors Token)
requestAnonymousToken =
  AX.request ((pgrequest (createURL "/rpc/get_token")) { method = Left POST, responseFormat = Res.string })
  <#> \{body} ->
    either
      (Left <<< pure <<< ForeignError <<< Res.printResponseFormatError)
      (\s -> pt s
        >>= (A.head >>> note (pure (ForeignError "No first item in array found.")))
        >>= \t -> parseToken t.token
      )
      body
  where
    pt :: String -> Either MultipleErrors (Array { token :: String })
    pt = readJSON
