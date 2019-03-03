module Types.Agenda
  ( AgendaItem(..)
  , Agenda
  , _AgendaItems
  , _SpeakerQueues
  , topSQ
  , pushSQ
  , popSQIfMatchingId
  , next
  , prev
  , empty
  , insert
  , jumpToFirstActive
  , getCurrentAI
  ) where

import Types.SpeakerQueue

import Control.Alternative ((<|>))
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Lens (Lens', element, lens, over, preview, traverseOf, traversed)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, readArray)
import Prelude (class Eq, class Ord, class Show, compare, const, eq, otherwise, pure, show, (&&), (*>), (+), (-), (/=), (<#>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>))
import Record as R
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))
import Types.Lens (_withId)

------------------------------------------------------------------------------
-- | AgendaItem
------------------------------------------------------------------------------
newtype AgendaItem = AgendaItem
  { id            :: Int
  , title         :: String
  , order_        :: Int
  , parent        :: Maybe AgendaItem
  , state         :: String
  , speakerQueues :: L.List SpeakerQueue
  }
derive instance ntAI :: Newtype AgendaItem _
derive instance eqAI :: Eq      AgendaItem

instance shAI :: Show AgendaItem where
  show (AgendaItem ai) = "AgendaItem " <> show ai

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

_SpeakerQueues :: Lens' AgendaItem (L.List SpeakerQueue)
_SpeakerQueues
  = lens
      (\(AgendaItem a)    -> a.speakerQueues)
      (\(AgendaItem a) ss -> AgendaItem a { speakerQueues = ss })

-- | The top of the speaker queue stack.
topSQ :: AgendaItem -> Maybe SpeakerQueue
topSQ = preview (_SpeakerQueues <<< traversed)

-- | Push a speakerqueue onto the top of the stack.
pushSQ :: SpeakerQueue -> AgendaItem -> AgendaItem
pushSQ sq = over _SpeakerQueues (L.Cons sq)

-- | If the ids match, pop the speakerQueue and return Just the new AgendaItem, otherwise, noop.
popSQIfMatchingId :: Int -> AgendaItem -> Maybe AgendaItem
popSQIfMatchingId i ai
  =  preview (element 0 _SpeakerQueues <<< _withId i) ai
  *> traverseOf _SpeakerQueues (L.tail) ai

------------------------------------------------------------------------------
-- | Agenda
------------------------------------------------------------------------------
data Agenda = Agenda Int (Array AgendaItem)

type RawAgendaItem =
  { id            :: Int
  , title         :: String
  , order_        :: Int
  , parent        :: Maybe Int
  , state         :: String
  , speakerQueues :: Array SpeakerQueue
  }

getId :: Foreign -> F {id :: Int}
getId = readImpl

getParent :: Foreign -> F {parent :: Maybe Int}
getParent = readImpl

instance rfAg :: ReadForeign Agenda where
  readImpl =
    readArray
      >=> traverse (\fr -> (\{parent} -> {parent, fr}) <$> getParent fr)
      >=> (A.partition (\{parent} -> isNothing parent)
        >>> \{yes, no} ->
              let forest =
                          (M.fromFoldableWith
                            (<>)
                            (A.mapMaybe
                              (\{fr, parent} -> (\p -> Tuple p [fr]) <$> parent)
                              no)) :: M.Map Int (Array Foreign)
               in A.concat <$> for yes (\{fr} ->
                 readImpl fr
                   >>= (\parent@(AgendaItem {id}) ->
                       maybe
                         (pure [parent])
                         (traverse (\x ->
                             (readImpl x :: F RawAgendaItem)
                               <#> R.set (SProxy :: SProxy "parent") (Just parent)
                               >>> R.modify
                                     (SProxy :: SProxy "speakerQueues")
                                     (L.sort <<< (L.fromFoldable :: Array SpeakerQueue -> L.List SpeakerQueue))
                               >>> AgendaItem
                           )
                         )
                         (M.lookup id forest)
                      )))
      >=> (pure <<< Agenda 0)

instance showAgenda :: Show Agenda where
  show (Agenda i xs) = "Agenda " <> show i <> "\n" <> show xs

instance equalAgenda :: Eq Agenda where
  eq (Agenda i1 xs1) (Agenda i2 xs2) = eq i1 i2 && eq xs1 xs2

-- | How to get insight into the agenda.
_AgendaItems :: Lens' Agenda (Array AgendaItem)
_AgendaItems
  = lens
      (\(Agenda _ items)       -> items)
      (\(Agenda i _    ) items -> Agenda i items)

-- | Set the internal index to the first argument, and return Just the Agenda.
-- | If out of bounds, return Nothing.
setIdx :: Int -> Agenda -> Maybe Agenda
setIdx i (Agenda _ as) = A.index as i <#> const (Agenda i as)

-- | The next agenda item.
next :: Agenda -> Maybe Agenda
next a@(Agenda i _) = setIdx (i+1) a

-- | The previous agenda item.
prev :: Agenda -> Maybe Agenda
prev a@(Agenda i _) = setIdx (i-1) a

-- | And empty agenda.
empty :: Agenda
empty = Agenda 0 []

-- | Insert an agenda item.
insert :: AgendaItem -> Agenda -> Agenda
insert ai = over _AgendaItems (A.insert ai)

-- | This is an interesting beast...
-- | It jumps to first active or last agenda item.
-- | It does nothing if the agenda is empty.
jumpToFirstActive :: Agenda -> Agenda
jumpToFirstActive noop@(Agenda _ xs)
  | A.null xs = noop
  | otherwise = Agenda i xs
    where
     i = fromMaybe ((A.length xs) - 1)
          (A.findIndex (\(AgendaItem a) -> a.state == "active") xs
          <|> A.findIndex (\(AgendaItem a) -> a.state /= "done") xs)

getCurrentAI :: Agenda -> Either String AgendaItem
getCurrentAI (Agenda i xs)
  | A.null xs = Left "ERROR: There is no current agenda item."
  | otherwise = note "ERROR: Current agenda item index out of bounds." (A.index xs i)
