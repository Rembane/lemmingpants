module Types.SpeakerQueue
  ( SpeakerQueue(..)
  , SpeakerQueueRecord
  , SpeakerQueueState(..)
  , _Speaking
  , _Speakers
  ) where

import Control.Monad.Except (throwError)
import Data.Array (filter, index, sort)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism', lens, over, prism')
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..))
import Prelude (class Eq, class Ord, class Show, compare, identity, pure, show, ($), (&&), (/=), (<#>), (<>), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Types.Speaker (Speaker(..))
import Types.Speaker as S

data SpeakerQueueState
  = Init
  | Active
  | Done

derive instance eqSQS  :: Eq      SpeakerQueueState
derive instance genSQS :: Generic SpeakerQueueState _

instance arbSQS :: Arbitrary SpeakerQueueState where
  arbitrary = genericArbitrary

instance shSQS :: Show SpeakerQueueState where
  show = case _ of
           Init    -> "init"
           Active  -> "active"
           Done    -> "done"

instance rfSQS :: ReadForeign SpeakerQueueState where
  readImpl fr
    =   readImpl fr
    >>= case _ of
          "init"    -> pure Init
          "active"  -> pure Active
          "done"    -> pure Done
          e         -> throwError $ pure $ ForeignError $ "This is not a SpeakerQueueState: " <> e

type SpeakerQueueRecord =
  { id           :: Int
  , state        :: SpeakerQueueState
  , speakers     :: Array Speaker
  , speakerAdded :: Maybe Speaker
  }
newtype SpeakerQueue = SpeakerQueue SpeakerQueueRecord
derive instance ntSQ  :: Newtype SpeakerQueue _
derive instance eqSQ  :: Eq      SpeakerQueue
derive instance genSQ :: Generic SpeakerQueue _

instance arbSQ :: Arbitrary SpeakerQueue where
  arbitrary = genericArbitrary

instance shSQ :: Show SpeakerQueue where
  show (SpeakerQueue sq) = "SpeakerQueue " <> show sq

-- | Applying the setter for _Speakers is the same as
-- | applying the deprecated invariantDance to a SpeakerQueue.
-- | This should uphold the invariants.
instance rfSQ :: ReadForeign SpeakerQueue where
  readImpl fr
    =   readImpl fr
    <#> SpeakerQueue
    >>> (over _Speakers identity)

-- | The speaker queue with the highest id should be on top of the stack.
-- | This will let us work our way down to the first speaker queue by
-- | popping speaker queues from the stack.
instance ordSQ :: Ord SpeakerQueue where
  compare (SpeakerQueue s1) (SpeakerQueue s2) =
    compare s2.id s1.id

_Speakers :: Lens' SpeakerQueue (Array Speaker)
_Speakers =
  lens
    (\(SpeakerQueue {speakers}) -> speakers)
    (\(SpeakerQueue r) s' ->
      let f = filter
                (\(Speaker {state}) -> state /= S.Done && state /= S.Deleted)
              >>> sort
       in SpeakerQueue r { speakers = f s' })

-- | An optic that focuses on someone speaking.
_Speaking :: Prism' (Array Speaker) Speaker
_Speaking
  = prism'
      (pure)
      (\a -> index a 0
        >>= \s@(Speaker {state}) ->
                case state of
                  S.Active -> Just s
                  _        -> Nothing
      )
