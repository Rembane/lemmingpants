{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB
    ( Database
    , empty
    , save
    , load
    , createAttendee
    , listAttendees
    , getAgendaItem
    , listAgendaItems
    ) where

import qualified Data.ByteString as B
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Serialize (Serialize, decode, encode)
import Data.Vector.Serialize ()
import Data.Serialize.Text ()
import Data.Text hiding (empty)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Types

data Database = Database
    { attendees     :: M.Map Text Attendee -- ^ Search by CID.
    , attendeeIndex :: Int
    , agenda        :: V.Vector AgendaItem
    } deriving (Generic)

instance Serialize Database

empty :: Database
empty = Database
    { attendees     = M.empty
    , attendeeIndex = 1
    , agenda        = V.empty
    }

-- | Save database to disk.
save :: String -> TVar Database -> IO ()
save fn db = (B.writeFile fn . encode) =<< readTVarIO db

-- | Load database from disk.
load :: String -> IO (Either String Database)
load = (fmap . fmap) decode B.readFile

-- | Creates an attendee and puts it in the database.
-- If the attendee already exists, return it instead.
-- Takes a CID as first argument.
createAttendee :: Text -> TVar Database -> STM Attendee
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

-- | Get an agenda item by id.
getAgendaItem :: Int -> TVar Database -> STM (Maybe AgendaItem)
getAgendaItem id' db = (V.!? id') . agenda <$> readTVar db

-- | List agenda items.
listAgendaItems :: TVar Database -> STM [AgendaItem]
listAgendaItems db = V.toList . agenda <$> readTVar db
