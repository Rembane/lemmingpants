module Postgrest
  ( post
  , withSignedIn
  ) where

-- | Here we keep the functions to make our interactions with Postgrest a breeze.

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.StrMap (StrMap)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (pure, (<$>), (<>))
import Simple.JSON (writeJSON)

pgrequest :: String -> StrMap String -> AX.AffjaxRequest String
pgrequest url d = AX.defaultRequest
  { url     = url
  , headers =
      AX.defaultRequest.headers <> [ ContentType (MediaType "application/json") ]
  , method  = Left POST
  , content = Just (writeJSON d)
  }

post
  :: forall a e
   . Respondable a
  => String
  -> StrMap String
  -> Aff (ajax :: AJAX | e) (AX.AffjaxResponse a)
post url d = AX.affjax (pgrequest url d)

withSignedIn
  :: forall a e
   . Respondable a
  => String
  -> Maybe String
  -> StrMap String
  -> Aff (ajax :: AJAX | e) (Either String (AX.AffjaxResponse a))
withSignedIn url token d =
  case token of
    Nothing -> pure (Left "You are not logged in. Please login.")
    Just t  ->
      let pg' = pgrequest url d
       in Right <$> AX.affjax
            (pg'
               { headers =
                   pg'.headers <> [ RequestHeader "Authorization" ("Bearer " <> t) ]
               })
