module Types.Attendee
  (Attendee(..)
  ) where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Prelude (class Eq)
import Simple.JSON (class ReadForeign)

newtype Attendee = Attendee
  { id   :: Int
  , cid  :: String
  , name :: String
  , nick :: Maybe String
  }
derive instance         eqAt :: Eq          Attendee
derive instance         ntAt :: Newtype     Attendee _
derive newtype instance rfAt :: ReadForeign Attendee
