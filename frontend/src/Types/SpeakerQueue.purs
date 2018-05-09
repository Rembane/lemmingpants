module Types.SpeakerQueue
  ( SpeakerQueue(..)
  , SpeakerQueueRecord
  , addSpeaker
  , modifySpeaker
  ) where

import Control.Alt ((<|>))
import Data.Array as A
import Data.Lens (Lens', _Just, element, filtered, findOf, lens, over, preview, to, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Record (modify)
import Data.Record.ShowRecord (showRecord)
import Prelude (class Eq, class Ord, class Show, compare, id, ($), (&&), (/=), (<#>), (<<<), (<>), (==), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))
import Types.Speaker (Speaker(..))

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
      >>> invariantDance

-- | The speaker queue with the highest id should be on top of the stack.
-- | This will let us work our way down to the first speaker queue by
-- | popping speaker queues from the stack.
instance ordSQ :: Ord SpeakerQueue where
  compare (SpeakerQueue s1) (SpeakerQueue s2) =
    compare s2.id s1.id

_Speaking :: Lens' SpeakerQueue (Maybe Speaker)
_Speaking =
  lens
    (\(SpeakerQueue {speaking}) -> speaking)
    (\(SpeakerQueue r) s' -> SpeakerQueue (r { speaking = s' }))

_Speakers :: Lens' SpeakerQueue (Array Speaker)
_Speakers =
  lens
    (\(SpeakerQueue {speakers}) -> speakers)
    (\(SpeakerQueue r) s' -> SpeakerQueue (r { speakers = s' }))

-- | Make sure that no speaker on the `speaking` position is done.
-- | And that no speaker in the queue is done or deleted.
-- | And that no speaker first in the queue is active.
invariantDance :: SpeakerQueue -> SpeakerQueue
invariantDance = moveActive <<< removeDone
  where
    removeDone :: SpeakerQueue -> SpeakerQueue
    removeDone
      =   over _Speakers (A.filter (\(Speaker s) -> s.state /= "done" && s.state /= "deleted"))
      >>> over _Speaking (preview (_Just <<< filtered (\(Speaker s') -> s'.state /= "done")))

    moveActive :: SpeakerQueue -> SpeakerQueue
    moveActive sq =
      fromMaybe sq $ A.uncons (view _Speakers sq)
        >>= \{head, tail} ->
          let s@(Speaker s') = head
           in if s'.state == "active"
                then Just (over _Newtype (\sq' -> sq' { speaking = Just s, speakers = tail }) sq)
                else Nothing

addSpeaker :: Speaker -> SpeakerQueue -> SpeakerQueue
addSpeaker s = invariantDance <<< over _Speakers (A.insert s)

-- | Modify the speaker with the id that is the first argument of this function.
-- | Do nothing if the speaker isn't found.
modifySpeaker :: Int -> (Speaker -> Speaker) -> SpeakerQueue -> SpeakerQueue
modifySpeaker id_ f sq = invariantDance $ fromMaybe sq r
  where
    equalId (Speaker s) = s.id == id_

    r = over _Speakers (\ss -> over (element (A.findIndex equalId ss) ss) f) sq -- findOf _Speakers equalId sq
      -- >>= view (_Newtype <<< to (\s -> s.id))

{-
      (A.findIndex equalId sq.speakers
        >>= (\i -> A.modifyAt i f sq.speakers)
        <#> (\ss' -> sq { speakers = ss' }))
      <|>
      (sq.speaking >>= g)
      -}

    {-
    g :: Speaker -> Maybe SpeakerQueueRecord
    g s | equalId s = Just $ sq { speaking = Just (f s) }
        | true      = Nothing

-}
