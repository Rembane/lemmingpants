module Types where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Record (modify)
import Data.Record.ShowRecord (showRecord)
import Data.Sequence.Ordered (fromFoldable)
import Data.Sequence.Ordered as OrdSeq
import Prelude (class Eq, class Ord, class Show, compare, (<#>), (<>), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))

type SpeakerRecord =
  { id       :: Int
  , state    :: String
  , speaking :: Maybe Speaker
  , speakers :: OrdSeq.OrdSeq Speaker
  }

newtype SpeakerQueue = SpeakerQueue SpeakerRecord
derive instance ntSQ :: Newtype SpeakerQueue _
derive instance eqSQ :: Eq SpeakerQueue

instance shSQ :: Show SpeakerQueue where
  show (SpeakerQueue sq) = "SpeakerQueue " <> showRecord sq

instance rfSQ :: ReadForeign SpeakerQueue where
  readImpl fr =
    readImpl fr
      <#> modify
            (SProxy :: SProxy "speakers")
            (fromFoldable :: Array Speaker -> OrdSeq.OrdSeq Speaker)
      >>> SpeakerQueue

-- | The speaker queue with the highest id should be on top of the stack.
-- | This will let us work our way down to the first speaker queue by
-- | popping speaker queues from the stack.
-- |
-- | Since we use an ordered sequence as underlying representation
-- | the top of the stack is the greatest element. Oh the joy of
-- | leaky abstractions.
instance ordSQ :: Ord SpeakerQueue where
  compare (SpeakerQueue s1) (SpeakerQueue s2) =
    compare s1.id s2.id

modifySpeakerQueue :: (SpeakerRecord -> SpeakerRecord) -> SpeakerQueue -> SpeakerQueue
modifySpeakerQueue f (SpeakerQueue sq) = SpeakerQueue (f sq)

newtype Attendee = Attendee
  { id   :: Int
  , cid  :: String
  , name :: String
  , nick :: Maybe String
  }
derive instance eqAt :: Eq Attendee
derive instance ntAt :: Newtype Attendee _
derive newtype instance rfAt :: ReadForeign Attendee

newtype Speaker = Speaker
  { id          :: Int
  , attendeeId  :: Int
  , state       :: String
  , timesSpoken :: Int
  }
derive instance ntSp :: Newtype Speaker _
derive newtype instance rfSp :: ReadForeign Speaker
derive instance eqSp :: Eq Speaker

instance shSp :: Show Speaker where
  show (Speaker s) = "Speaker " <> showRecord s

instance ordSp :: Ord Speaker where
  compare (Speaker s1) (Speaker s2) =
    compare s1.timesSpoken s2.timesSpoken <> compare s1.id s2.id

newtype AgendaItem = AgendaItem
  { id            :: Int
  , title         :: String
  , content       :: String
  , order_        :: Int
  , state         :: String
  , speakerQueues :: OrdSeq.OrdSeq SpeakerQueue
  }
derive instance ntAI :: Newtype AgendaItem _
derive instance eqAI :: Eq AgendaItem

instance shAI :: Show AgendaItem where
  show (AgendaItem ai) = "AgendaItem " <> showRecord ai

instance rfAI :: ReadForeign AgendaItem where
  readImpl fr =
    readImpl fr
      <#> modify
            (SProxy :: SProxy "speakerQueues")
            (fromFoldable :: Array SpeakerQueue -> OrdSeq.OrdSeq SpeakerQueue)
      >>> AgendaItem
