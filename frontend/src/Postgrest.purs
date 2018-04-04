module Postgrest
  ( post
  , signedInAjax
  , useToken
  ) where

-- | Here we keep the functions to make our interactions with Postgrest a breeze.

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.MediaType (MediaType(..))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude ((<>))
import Simple.JSON (class WriteForeign, writeJSON)

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
  :: Maybe String
  -> Either String RequestHeader
useToken token =
  case token of
    Nothing -> Left "You are not logged in. Please login."
    Just t  -> Right (RequestHeader "Authorization" ("Bearer " <> t))

signedInAjax
  :: forall a d e
   . Respondable a
  => WriteForeign d
  => String
  -> Maybe String
  -> Method
  -> Array RequestHeader
  -> d
  -> Aff (ajax :: AJAX | e) (Either String (AX.AffjaxResponse a))
signedInAjax url token m hs d =
  traverse
    (\r ->
      let pg' = pgrequest url d
       in AX.affjax (pg' { headers = pg'.headers <> [r] <> hs, method = Left m })
    )
    (useToken token)
