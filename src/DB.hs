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
    , listAgendaItems

{-
    , pushSpeakerQueue
    , popSpeakerQueue
    , enqueueSpeaker
    , dequeueSpeaker
    -}
    ) where

import qualified Data.ByteString as B
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.List (sortBy)
import Data.Serialize (Serialize, decode, encode)
import Data.Vector.Serialize ()
import Data.Serialize.Text ()
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

import Types

data Database = Database
    { attendees     :: M.Map T.Text Attendee -- ^ Find by CID.
    , attendeeIndex :: Int
    , agenda        :: M.Map UUID AgendaItem -- ^ Find by UUID.
    } deriving (Generic)

instance Serialize Database

empty :: Database
empty = Database
    { attendees     = M.empty
    , attendeeIndex = 1
    , agenda        = M.empty
    }

-- | Save database to disk.
save :: String -> TVar Database -> IO ()
save fn db = (B.writeFile fn . encode) =<< readTVarIO db

-- | Load database from disk.
load :: String -> IO (Either String Database)
load = (fmap . fmap) decode B.readFile

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
               , speakerQueueStack = []
               , order             = o
               }

-- | Get an agenda item by id.
getAgendaItem :: UUID -> TVar Database -> STM (Maybe AgendaItem)
getAgendaItem uuid db = M.lookup uuid . agenda <$> readTVar db

-- | List agenda items.
listAgendaItems :: TVar Database -> STM [AgendaItem]
listAgendaItems db = sortBy (compare `on` order) . Prelude.map snd . M.toList . agenda <$> readTVar db

