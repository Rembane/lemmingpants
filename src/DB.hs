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

    , createAgendaItem
    , getAgendaItem
    , modifyAgendaItem
    , listAgendaItems

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
import Data.Monoid ((<>))
import Control.Monad.STM
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.List (sortBy, uncons)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.UUID (UUID)
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
    , agenda        :: M.Map UUID AgendaItem -- ^ Find by UUID.
    } deriving (FromJSON, Generic, ToJSON)

empty :: Database
empty = Database
    { attendees = Attendees M.empty M.empty 0
    , agenda    = M.empty
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
          let a                = Attendee n cid
              n                = 1 + idx atts
           in writeTVar db (db' { attendees = atts { byCID = M.insert cid a (byCID atts)
                                                   , byId  = M.insert n   a (byId  atts)
                                                   } } ) >> return a
       Just  a -> return a

-- | Returns a list of all attendees in arbitrary order.
listAttendees :: TVar Database -> STM [Attendee]
listAttendees db = M.elems . byId . attendees <$> readTVar db

-- | Create an item on the agenda.
-- Takes a UUID as first argument.
createAgendaItem :: UUID -> Int -> T.Text -> T.Text -> TVar Database -> STM AgendaItem
createAgendaItem uuid o t c db = go =<< readTVar db
    where
        go db' = writeTVar db (db' { agenda = M.insert uuid a (agenda db') }) >> return a
        a      = AgendaItem
               { id                = uuid
               , title             = t
               , content           = c
               , speakerQueueStack = [SpeakerQueue Seq.empty Nothing]
               , order             = o
               }

-- | Get an agenda item by id.
getAgendaItem :: UUID -> TVar Database -> STM (Maybe AgendaItem)
getAgendaItem uuid db = M.lookup uuid . agenda <$> readTVar db

-- | List agenda items.
listAgendaItems :: TVar Database -> STM [AgendaItem]
listAgendaItems db = sortBy (compare `on` order) . Prelude.map snd . M.toList . agenda <$> readTVar db

-- | Modify an agenda item using a higher order function.
-- Returns Nothing if the agenda item couldn't be found.
modifyAgendaItem :: (AgendaItem -> AgendaItem) -> UUID -> TVar Database -> STM (Maybe AgendaItem)
modifyAgendaItem f uuid db = do
    a <- (fmap . fmap) f (getAgendaItem uuid db)
    case a of
      Nothing -> return Nothing
      Just a' -> do
          db' <- readTVar db
          writeTVar db (db' { agenda = M.insert uuid a' (agenda db')})
          return (Just a')

-- | Push a speakerqueue onto the speakerQueueStack.
pushSpeakerQueue :: UUID -> TVar Database -> STM (Maybe AgendaItem)
pushSpeakerQueue uuid db = modifyAgendaItem (\a -> a { speakerQueueStack = (SpeakerQueue Seq.empty Nothing) : speakerQueueStack a }) uuid db

-- | Pop a speakerqueue from the speakerQueueStack and throw it away.
popSpeakerQueue :: UUID -> TVar Database -> STM (Maybe AgendaItem)
popSpeakerQueue uuid db = modifyAgendaItem (\a -> a { speakerQueueStack = go a }) uuid db
    where
        go a = case concat $ maybeToList $ fmap snd $ uncons $ speakerQueueStack a of
                 [] -> [SpeakerQueue Seq.empty Nothing]
                 xs -> xs

-- | Modify the top element of the speaker queue stack.
modifyTopOfStack :: (SpeakerQueue -> SpeakerQueue) -> UUID -> TVar Database -> STM (Maybe AgendaItem)
modifyTopOfStack f = modifyAgendaItem (\a -> a {speakerQueueStack = go $ speakerQueueStack a })
    where
        go (x:xs) = f x : xs

-- | Find a speaker by Id in the database.
lookupSpeaker :: Int -> TVar Database -> STM (Maybe Attendee)
lookupSpeaker id' db = (M.lookup id' . byId . attendees) <$> readTVar db

-- | Enqueue a speaker on the topmost speaker queue.
-- TODO: Fix the weird error, we must have to error messages here.
enqueueSpeaker :: Int -> UUID -> TVar Database -> STM (Maybe AgendaItem)
enqueueSpeaker id' uuid db = do
    db' <- readTVar db
    s   <- lookupSpeaker id' db
    case s of
      Nothing -> return Nothing
      Just s' -> modifyTopOfStack (go s') uuid db

    where
        go :: Attendee -> SpeakerQueue -> SpeakerQueue
        go a s = s { speakers = (Seq.|> a) $ speakers s }

-- | Dequeue the first speaker on the topmost speaker queue and set
-- the speaker as the current speaker.
dequeueSpeaker :: UUID -> TVar Database -> STM (Maybe AgendaItem)
dequeueSpeaker = modifyTopOfStack go
    where
        go :: SpeakerQueue -> SpeakerQueue
        go s = case Seq.viewl $ speakers s of
                 Seq.EmptyL  -> SpeakerQueue Seq.empty Nothing
                 a Seq.:< s' -> s { speakers = s', current = Just a }

-- | Remove all the occurences of a speaker from the topmost speaker queue.
removeSpeaker :: Int -> UUID -> TVar Database -> STM (Maybe AgendaItem)
removeSpeaker id' = modifyTopOfStack (go id')
    where
        go :: Int -> SpeakerQueue -> SpeakerQueue
        go i s = s { speakers = Seq.filter (\a -> aid a /= i) $ speakers s }

        aid :: Attendee -> Int
        aid = Types.id
