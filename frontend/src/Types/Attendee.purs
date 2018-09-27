module Types.Attendee
  (Attendee(..)
  , visualizeAttendee
  , AttendeeDB
  , newAttendeeDB
  , getAttendeeById
  , getAttendeeByNumber
  , insertAttendee
  , listAttendees
  ) where

import Data.Array as A
import Data.Foldable (foldr)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, map, ($), (<#>), (<>), (>>=), (>>>))
import Record (modify)
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))

newtype Attendee = Attendee
  { id      :: Int
  , cid     :: String
  , name    :: String
  , nick    :: Maybe String
  , numbers :: Array Int
  }
derive instance eqAt :: Eq      Attendee
derive instance ntAt :: Newtype Attendee _

instance rfAt :: ReadForeign Attendee where
  readImpl fr =
    readImpl fr
      <#> modify
          (SProxy :: SProxy "numbers")
          go
      >>> Attendee
    where
      go :: Array { id :: Int } -> Array Int
      go = map \{id} -> id

visualizeAttendee :: Attendee -> String
visualizeAttendee (Attendee a) =
  case a.nick of
    Nothing -> a.name
    Just n  ->
      case A.uncons (split (Pattern " ") a.name) of
        Nothing           -> a.name
        Just {head, tail} -> A.intercalate " " ([head] <> ["”" <> n <> "”"] <> tail)

newtype AttendeeDB = AttendeeDB
  { idToAttendee :: M.Map Int Attendee
  , numberToId   :: M.Map Int Int
  }

newAttendeeDB :: Array Attendee -> AttendeeDB
newAttendeeDB as = AttendeeDB
  { idToAttendee : M.fromFoldable (map (\a@(Attendee a') -> Tuple a'.id a) as)
  , numberToId   : M.fromFoldable (foldr (\(Attendee a) acc -> acc <> map (\n -> Tuple n a.id) a.numbers) mempty as)
  }

getAttendeeById :: Int -> AttendeeDB -> Maybe Attendee
getAttendeeById i (AttendeeDB a) = M.lookup i a.idToAttendee

getAttendeeByNumber :: Int -> AttendeeDB -> Maybe Attendee
getAttendeeByNumber i a@(AttendeeDB a') =
  M.lookup i a'.numberToId >>= (\i' -> getAttendeeById i' a)

insertAttendee :: Attendee -> AttendeeDB -> AttendeeDB
insertAttendee a@(Attendee a') (AttendeeDB db) = AttendeeDB $
  db { idToAttendee = M.insert a'.id a db.idToAttendee
     , numberToId   = foldr (\n acc -> M.insert n a'.id acc) db.numberToId a'.numbers
     }

listAttendees :: AttendeeDB -> L.List Attendee
listAttendees (AttendeeDB {idToAttendee}) = M.values idToAttendee
