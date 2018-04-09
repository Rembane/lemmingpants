module Types.SpeakerQueue
  ( SpeakerQueue(..)
  , SpeakerQueueRecord
  , invariantDance
  , addSpeaker
  , modifySpeaker
  ) where

import Control.Alt ((<|>))
import Data.Array as A
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Record (modify)
import Data.Record.ShowRecord (showRecord)
import Prelude (class Eq, class Ord, class Show, compare, ($), (/=), (<#>), (<<<), (<>), (==), (>>=), (>>>))
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

-- | Make sure that no speaker on the `speaking` position is done.
-- | And that no speaker in the queue is done.
-- | And that no speaker first in the queue is active.
invariantDance :: SpeakerQueue -> SpeakerQueue
invariantDance = moveActive <<< removeDone
  where
    removeDone :: SpeakerQueue -> SpeakerQueue
    removeDone (SpeakerQueue sq) =
      SpeakerQueue $ sq { speaking = sq.speaking >>= (\s@(Speaker s') -> if s'.state == "done" then Nothing else Just s)
                        , speakers = A.filter (\(Speaker x) -> x.state /= "done") sq.speakers
                        }

    moveActive :: SpeakerQueue -> SpeakerQueue
    moveActive (SpeakerQueue sq) =
      case A.uncons sq.speakers of
        Nothing           -> SpeakerQueue sq
        Just {head, tail} ->
          let s@(Speaker s') = head
           in if s'.state == "active"
                then SpeakerQueue (sq { speaking = Just s, speakers = tail })
                else SpeakerQueue sq

addSpeaker :: Speaker -> SpeakerQueue -> SpeakerQueue
addSpeaker s (SpeakerQueue sq) = invariantDance $ SpeakerQueue $ sq { speakers = A.insert s sq.speakers }

-- | The first argument is the _id_ of the speaker.
modifySpeaker :: Int -> (Speaker -> Speaker) -> SpeakerQueue -> SpeakerQueue
modifySpeaker id_ f (SpeakerQueue sq) = invariantDance $ SpeakerQueue $ fromMaybe sq r
  where
    equalId (Speaker s) = s.id == id_

    r =
      ((A.findIndex equalId sq.speakers
        >>= (\i -> A.modifyAt i f sq.speakers)
        <#> (\ss' -> sq { speakers = ss' }))
      <|>
      (sq.speaking >>= g))

    g :: Speaker -> Maybe SpeakerQueueRecord
    g s | equalId s = Just $ sq { speaking = Just (f s) }
        | true      = Nothing

