{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.List (sortBy, uncons)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

import Types

data Database = Database
    { attendees     :: M.Map T.Text Attendee -- ^ Find by CID.
    , attendeeIndex :: Int
    , agenda        :: M.Map UUID AgendaItem -- ^ Find by UUID.
    } deriving (FromJSON, Generic, ToJSON)

empty :: Database
empty = Database
    { attendees     = M.empty
    , attendeeIndex = 1
    , agenda        = M.empty
    }

-- | Save database to disk.
save :: String -> TVar Database -> IO ()
save fn db = (BL.writeFile fn . encode) =<< readTVarIO db

-- | Load database from disk.
load :: String -> IO (Either String Database)
load = (fmap . fmap) eitherDecodeStrict' B.readFile

-- | Dies horribly if database can't be loaded.
loadOrDie :: String -> IO DB.Database
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
    case M.lookup cid (attendees db') of
      Nothing -> let n = 1 + attendeeIndex db'
                     a = Attendee n cid
                  in writeTVar db (db' { attendeeIndex = n, attendees = M.insert cid a (attendees db') }) >> return a
      Just a  -> return a

-- | Returns a list of all attendees in arbitrary order.
listAttendees :: TVar Database -> STM [Attendee]
listAttendees db = M.elems . attendees <$> readTVar db

-- | Create an item on the agenda.
-- Takes a UUID as first argument.
createAgendaItem :: UUID -> Int -> T.Text -> T.Text -> TVar Database -> STM AgendaItem
createAgendaItem uuid o t c db = readTVar db >>= go
    where
        go db' = writeTVar db (db' { agenda = M.insert uuid a (agenda db') }) >> return a
        a      = AgendaItem
               { id                = uuid
               , title             = t
               , content           = c
               , speakerQueueStack = [SpeakerQueue V.empty]
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
pushSpeakerQueue = modifyAgendaItem (\a -> a { speakerQueueStack = (SpeakerQueue V.empty) : speakerQueueStack a })

-- | Pop a speakerqueue from the speakerQueueStack and throw it away.
popSpeakerQueue :: UUID -> TVar Database -> STM (Maybe AgendaItem)
popSpeakerQueue = modifyAgendaItem (\a -> a { speakerQueueStack = go a })
    where
        go a = case concat $ maybeToList $ fmap snd $ uncons $ speakerQueueStack a of
                 [] -> [SpeakerQueue V.empty]
                 xs -> xs
