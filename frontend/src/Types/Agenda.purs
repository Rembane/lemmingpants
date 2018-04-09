module Types.Agenda
  ( AgendaItem(..)
  , Agenda
  , topSQ
  , pushSQ
  , popSQIfMatchingId
  , modifySQ
  , currIdx
  , curr
  , next
  , prev
  , empty
  , insert
  , modify
  , jumpToFirstActive
  , getCurrentAI
  ) where

import Types.SpeakerQueue

import Control.Alternative ((<|>))
import Data.Array as A
import Data.Either (Either, note)
import Data.List as L
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Record as R
import Data.Record.ShowRecord (showRecord)
import Prelude (class Eq, class Ord, class Show, compare, eq, (+), (-), (/=), (<), (<#>), (<<<), (<>), (==), (>=), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))

------------------------------------------------------------------------------
-- | AgendaItem
------------------------------------------------------------------------------

newtype AgendaItem = AgendaItem
  { id            :: Int
  , title         :: String
  , content       :: String
  , order_        :: Int
  , state         :: String
  , speakerQueues :: L.List SpeakerQueue
  }
derive instance ntAI :: Newtype AgendaItem _
derive instance eqAI :: Eq      AgendaItem

instance shAI :: Show AgendaItem where
  show (AgendaItem ai) = "AgendaItem " <> showRecord ai

instance ordAI :: Ord AgendaItem where
  compare (AgendaItem a1) (AgendaItem a2) =
    compare a1.order_ a2.order_

-- | `speakerQueues` is a stack and we put the largest element on the top.
-- | This should be excellent.
instance rfAI :: ReadForeign AgendaItem where
  readImpl fr =
    readImpl fr
      <#> R.modify
            (SProxy :: SProxy "speakerQueues")
            (L.sort <<< (L.fromFoldable :: Array SpeakerQueue -> L.List SpeakerQueue))
      >>> AgendaItem

-- The top of the speaker queue stack
topSQ :: AgendaItem -> Maybe SpeakerQueue
topSQ (AgendaItem a) = L.head a.speakerQueues

pushSQ :: SpeakerQueue -> AgendaItem -> AgendaItem
pushSQ sq (AgendaItem ai) = AgendaItem (ai { speakerQueues = L.Cons sq ai.speakerQueues })

-- | If the ids match, pop the speakerQueue, otherwise, noop.
popSQIfMatchingId :: Int -> AgendaItem -> Maybe AgendaItem
popSQIfMatchingId id (AgendaItem ai) =
  case L.uncons ai.speakerQueues of
    Nothing           -> Nothing
    Just {head, tail} -> let (SpeakerQueue sq) = head
                          in if sq.id == id
                               then Just (AgendaItem (ai { speakerQueues = tail }))
                               else Nothing

-- | We try to modify the speaker queue with the supplied id.
-- | If the speaker queue isn't found, we give back Nothing.
-- | And if you give us Nothing, we pass it on.
modifySQ :: Int -> (SpeakerQueue -> Maybe SpeakerQueue) -> AgendaItem -> Maybe AgendaItem
modifySQ i f (AgendaItem ai) =
  let {init, rest} = L.span (\(SpeakerQueue s) -> s.id /= i) ai.speakerQueues
   in case L.uncons rest of
        Nothing           -> Nothing
        Just {head, tail} ->
          f head >>=
            \f' -> Just (AgendaItem ai { speakerQueues = init <> L.Cons f' tail })

------------------------------------------------------------------------------
-- | Agenda
------------------------------------------------------------------------------
data Agenda = Agenda Int (Array AgendaItem)

instance rfAg :: ReadForeign Agenda where
  readImpl fr =
    readImpl fr
      <#> Agenda 0

currIdx :: Agenda -> Int
currIdx (Agenda i _) = i

curr :: Agenda -> Maybe AgendaItem
curr (Agenda i as) = A.index as i

next :: Agenda -> Maybe Agenda
next (Agenda i as)
  | (i+1) >= A.length as = Nothing
  | true                 = Just (Agenda (i+1) as)

prev :: Agenda -> Maybe Agenda
prev (Agenda i as)
  | (i-1) < 0 = Nothing
  | true      = Just (Agenda (i-1) as)

empty :: Agenda
empty = Agenda 0 []

insert :: AgendaItem -> Agenda -> Agenda
insert a (Agenda i as) = Agenda i (A.insert a as)

-- | We try to modify the agenda item with the same id.
-- | If the agenda item isn't found, we give back Nothing.
-- | If you change your mind, just return Nothing in the higher order
-- | function and we forget that it all happened.
modify :: Int -> (AgendaItem -> Maybe AgendaItem) -> Agenda -> Maybe Agenda
modify aid f (Agenda c as) =
  A.findIndex (\(AgendaItem a) -> eq aid a.id) as
    >>= \idx -> A.index as idx
    >>= f
    >>= \a -> A.updateAt idx a as
    <#> Agenda c

-- | This is an interesting beast...
jumpToFirstActive :: Agenda -> Agenda
jumpToFirstActive (Agenda _ as) = Agenda i as
  where
    i = fromMaybe ((A.length as) - 1)
      (A.findIndex (\(AgendaItem a) -> a.state == "active") as
      <|> A.findIndex (\(AgendaItem a) -> a.state /= "done") as)

getCurrentAI :: Agenda -> Either String AgendaItem
getCurrentAI =
  note "ERROR: There is no current agenda item." <<< curr

