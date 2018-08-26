module Types.Agenda
  ( AgendaItem(..)
  , Agenda
  , _AgendaItems
  , _CurrentAgendaItem
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
import Data.Either (Either, note)
import Data.Lens (Lens', element, lens, over, preview, traverseOf, traversed)
import Data.List as L
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Show, compare, const, show, ($), (*>), (+), (-), (/=), (<#>), (<$>), (<<<), (<>), (==), (>>>))
import Record as R
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))
import Types.Lens (_withId)

------------------------------------------------------------------------------
-- | AgendaItem
------------------------------------------------------------------------------

newtype AgendaItem = AgendaItem
  { id            :: Int
  , supertitle    :: Maybe String
  , title         :: String
  , order_        :: Int
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

instance rfAg :: ReadForeign Agenda where
  readImpl fr = Agenda 0 <$> readImpl fr

-- | How to get insight into the agenda.
_AgendaItems :: Lens' Agenda (Array AgendaItem)
_AgendaItems
  = lens
      (\(Agenda _ items)       -> items)
      (\(Agenda i _    ) items -> Agenda i items)

-- | The implementation of this is quite unsafe, but hopefully
-- | we can't break it. :D
-- | I think that setIdx makes sure that the invariants hold.
_CurrentAgendaItem :: Lens' Agenda AgendaItem
_CurrentAgendaItem
  = lens
      (\(Agenda i items)   -> unsafePartial $ fromJust $ A.index items i)
      (\(Agenda i items) a -> Agenda i $ unsafePartial $ fromJust $ A.updateAt i a items)

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
-- | It will crash horribly if the agenda is empty.
jumpToFirstActive :: Agenda -> Agenda
jumpToFirstActive (Agenda _ as) = Agenda i as
  where
    i = fromMaybe ((A.length as) - 1)
      (A.findIndex (\(AgendaItem a) -> a.state == "active") as
      <|> A.findIndex (\(AgendaItem a) -> a.state /= "done") as)

getCurrentAI :: Agenda -> Either String AgendaItem
getCurrentAI
  = note "ERROR: There is no current agenda item." <<< preview _CurrentAgendaItem
