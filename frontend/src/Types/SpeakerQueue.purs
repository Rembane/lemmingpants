module Types.SpeakerQueue
  ( SpeakerQueue(..)
  , SpeakerQueueRecord
  ) where

import Data.Array as A
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Record (modify)
import Data.Record.ShowRecord (showRecord)
import Prelude (class Eq, class Ord, class Show, compare, (<#>), (<>), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))
import Types.Speaker (Speaker)

type SpeakerQueueRecord =
  { id       :: Int
  , state    :: String
  , speaking :: Maybe Speaker
  , speakers :: Array Speaker
  }
newtype SpeakerQueue = SpeakerQueue SpeakerQueueRecord
derive instance ntSQ :: Newtype SpeakerQueue _
derive instance eqSQ :: Eq      SpeakerQueue

instance shSQ :: Show SpeakerQueue where
  show (SpeakerQueue sq) = "SpeakerQueue " <> showRecord sq

instance rfSQ :: ReadForeign SpeakerQueue where
  readImpl fr =
    readImpl fr
      <#> modify
            (SProxy :: SProxy "speakers")
            (A.sort)
      >>> SpeakerQueue

-- | The speaker queue with the highest id should be on top of the stack.
-- | This will let us work our way down to the first speaker queue by
-- | popping speaker queues from the stack.
-- |
-- | So why are these in order? Well, we can run `invert` when we put them
-- | in the stack.
instance ordSQ :: Ord SpeakerQueue where
  compare (SpeakerQueue s1) (SpeakerQueue s2) =
    compare s1.id s2.id
