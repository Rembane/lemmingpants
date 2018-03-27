module Queue
  ( Queue
  , empty
  , new
  , next
  , prev
  , getCurrent
  , modifyCurrent
  ) where

import Data.List (List(Nil), reverse, (:))
import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Show, show, (<>))

data Queue a = Queue (List a) (List a)

derive instance eqQ :: (Eq a) => Eq (Queue a)

instance shQ :: (Show a) => Show (Queue a) where
  show (Queue l1 l2) = "Queue: previous: " <> show l1 <> "\nNext: " <> show l2

empty :: forall a. Queue a
empty = Queue Nil Nil

new :: forall a. List a -> List a -> Queue a
new ps ns = Queue (reverse ps) ns

next :: forall a. Queue a -> Queue a
next (Queue ps Nil   ) = Queue ps     Nil
next (Queue ps (n:ns)) = Queue (n:ps) ns

prev :: forall a. Queue a -> Queue a
prev (Queue Nil    ns) = Queue Nil ns
prev (Queue (p:ps) ns) = Queue ps  (p:ns)

getCurrent :: forall a. Queue a -> Maybe a
getCurrent (Queue _ Nil  ) = Nothing
getCurrent (Queue _ (c:_)) = Just c

modifyCurrent :: forall a. (a -> a) -> Queue a -> Queue a
modifyCurrent _ q@(Queue _ Nil  )  = q
modifyCurrent f   (Queue p (n:ns)) = Queue p (f n:ns)
