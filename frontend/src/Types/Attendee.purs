module Types.Attendee
  (Attendee(..)
  , visualizeAttendee
  ) where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Prelude (class Eq, (<>))
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

visualizeAttendee :: Attendee -> String
visualizeAttendee (Attendee a) =
  case a.nick of
    Nothing -> a.name
    Just n  ->
      case A.uncons (split (Pattern " ") a.name) of
        Nothing           -> a.name
        Just {head, tail} -> A.intercalate " " ([head] <> ["”" <> n <> "”"] <> tail)
