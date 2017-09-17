{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module DB
    ( Database
    , empty
    , save
    , load
    , loadOrDie

    , createAttendee
    , listAttendees
    , getAttendee

    , getActiveAgendaItem
    , getFancyAgendaItem
    , previousAgendaItem
    , nextAgendaItem
    , createAgendaItem
    , modifyAgendaItem

    , pushSpeakerQueue
    , popSpeakerQueue
    , enqueueSpeaker
    , dequeueSpeaker
    , removeSpeaker
    ) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Monoid ((<>))
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.List (uncons)
import Data.Maybe (fromJust, maybeToList)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

import Types

-- | If you ever try to change an Attendee and expect that the change should
-- be reflected everywhere you need to use somekind of reference or other
-- interesting construct.
data Attendees = Attendees
    { byCID :: M.Map T.Text Attendee
    , byId  :: M.Map Int    Attendee
    , idx   :: Int
    } deriving (FromJSON, Generic, ToJSON)

data Database = Database
    { attendees     :: Attendees
    , agenda        :: Agenda
    } deriving (FromJSON, Generic, ToJSON)

empty :: Database
empty = Database
    { attendees = Attendees M.empty M.empty 0
    , agenda    = Agenda [] []
    }

-- | Save database to disk.
save :: String -> TVar Database -> IO ()
save fn db = (BL.writeFile fn . encode) =<< readTVarIO db

-- | Load database from disk.
load :: String -> IO (Either String Database)
load = (fmap . fmap) eitherDecodeStrict' B.readFile

-- | Dies horribly if database can't be loaded.
loadOrDie :: String -> IO Database
loadOrDie fn = do
    exists <- doesFileExist fn
    if exists
       then do
            db <- DB.load fn
            case db of
              Left  s   -> error s
              Right db' -> return db'
       else
            return DB.empty

-- | Creates an attendee and puts it in the database.
-- If the attendee already exists, return it instead.
-- Takes a CID as first argument.
createAttendee :: T.Text -> TVar Database -> STM Attendee
createAttendee cid db = do
    db' <- readTVar db
    let atts = attendees db'
    case M.lookup cid (byCID atts) of
       Nothing ->
          let a = Attendee n cid
              n = 1 + idx atts
           in writeTVar db (db' { attendees = atts { byCID = M.insert cid a (byCID atts)
                                                   , byId  = M.insert n   a (byId  atts)
                                                   , idx   = n
                                                   } } ) >> return a
       Just  a -> return a

-- | Returns a list of all attendees in arbitrary order.
listAttendees :: TVar Database -> STM [Attendee]
listAttendees db = M.elems . byId . attendees <$> readTVar db

getAttendee :: Int -> TVar Database -> STM (Maybe Attendee)
getAttendee i db = (M.lookup i . byId . attendees) <$> readTVar db

-- | Get the active agenda item.
getActiveAgendaItem :: TVar Database -> STM AgendaItem
getActiveAgendaItem db = (head . future . agenda) <$> readTVar db

-- | Get the active agenda item nicely formatted.
getFancyAgendaItem :: TVar Database -> STM FancyAgendaItem
getFancyAgendaItem db =
    mkFancyAgendaItem
        <$> (length . history . agenda <$> readTVar db)
        <*> DB.getActiveAgendaItem db

-- | Modify an agenda.
modifyAgenda :: (Agenda -> Agenda) -> TVar Database -> STM ()
modifyAgenda f db = modifyTVar' db (\db' -> (db' { agenda = f (agenda db') }))

-- | Sets the previous agenda item as the current one.
previousAgendaItem :: TVar Database -> STM AgendaItem
previousAgendaItem db = modifyAgenda go db >> getActiveAgendaItem db
    where
        go a = case history a of
                 []     -> a
                 (x:xs) -> a { history = xs, future = x : future a }

-- | Sets the next agenda item as the current one.
nextAgendaItem :: TVar Database -> STM AgendaItem
nextAgendaItem db = modifyAgenda go db >> getActiveAgendaItem db
    where
        go a = case future a of
                 []     -> a
                 [x]    -> a
                 (x:xs) -> a { history = x : history a, future = xs }

-- | Create an item on the agenda.
createAgendaItem :: T.Text -> T.Text -> TVar Database -> STM AgendaItem
createAgendaItem t c db = const a <$> modifyAgenda go db
    where
        go ag = ag { future = future ag ++ [a] }
        a = AgendaItem
          { title             = t
          , content           = c
          , speakerQueueStack = [SpeakerQueue Seq.empty Nothing]
          }

-- | Modify an agenda item using a higher order function.
-- Returns Nothing if the agenda item couldn't be found.
modifyAgendaItem :: (AgendaItem -> AgendaItem) -> TVar Database -> STM ()
modifyAgendaItem f db = void $ modifyAgenda (\ag -> let (x:xs) = future ag in ag { future = f x : xs }) db

-- | Push a speaker queue onto the speakerQueueStack.
pushSpeakerQueue :: TVar Database -> STM [SpeakerQueue]
pushSpeakerQueue db = modifyAgendaItem (\a -> a { speakerQueueStack = (SpeakerQueue Seq.empty Nothing) : speakerQueueStack a }) db >> (speakerQueueStack <$> getActiveAgendaItem db)

-- | Pop a speaker queue from the speakerQueueStack and throw it away.
popSpeakerQueue :: TVar Database -> STM [SpeakerQueue]
popSpeakerQueue db = modifyAgendaItem (\a -> a { speakerQueueStack = go a }) db >> (speakerQueueStack <$> getActiveAgendaItem db)
    where
        go a = case concat $ maybeToList $ fmap snd $ uncons $ speakerQueueStack a of
                 [] -> [SpeakerQueue Seq.empty Nothing]
                 xs -> xs

-- | Modify the top element of the speaker queue stack.
modifyTopOfStack :: (SpeakerQueue -> SpeakerQueue) -> TVar Database -> STM [SpeakerQueue]
modifyTopOfStack f db = modifyAgendaItem (\a -> a {speakerQueueStack = go $ speakerQueueStack a }) db >> (speakerQueueStack <$> getActiveAgendaItem db)
    where
        go (x:xs) = f x : xs

-- | Find a speaker by Id in the database.
lookupSpeaker :: Int -> TVar Database -> STM (Maybe Attendee)
lookupSpeaker id' db = (M.lookup id' . byId . attendees) <$> readTVar db

-- | Enqueue a speaker on the topmost speaker queue.
enqueueSpeaker :: Attendee -> TVar Database -> STM [SpeakerQueue]
enqueueSpeaker a db = modifyTopOfStack (\s -> s { speakers = (Seq.|> a) $ speakers s }) db

-- | Dequeue the first speaker on the topmost speaker queue and set
-- the speaker as the current speaker.
dequeueSpeaker :: TVar Database -> STM [SpeakerQueue]
dequeueSpeaker db = modifyTopOfStack go db
    where
        go :: SpeakerQueue -> SpeakerQueue
        go s = case Seq.viewl $ speakers s of
                 Seq.EmptyL  -> SpeakerQueue Seq.empty Nothing
                 a Seq.:< s' -> s { speakers = s', current = Just a }

-- | Remove all the occurences of a speaker from the topmost speaker queue.
removeSpeaker :: Attendee -> TVar Database -> STM [SpeakerQueue]
removeSpeaker a db = modifyTopOfStack (\s -> s { speakers = Seq.filter (/= a) $ speakers s }) db
