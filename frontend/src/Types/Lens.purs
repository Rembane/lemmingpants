module Types.Lens
  ( _withId
  ) where

import Data.Foldable (class Foldable)
import Data.Lens (Traversal', filtered, traversed)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable)
import Prelude (class Applicative, (<<<), (==))

-- | For a Newtype in a Traversable where the record in the newtype has an id that has the right value.
_withId
  :: forall f fs n
   . Newtype n { id :: Int | fs }
  => Traversable f
  => Foldable f
  => Applicative f
  => Int
  -> Traversal' (f n) n
_withId i = traversed <<< filtered ((\a -> a.id == i) <<< unwrap)
