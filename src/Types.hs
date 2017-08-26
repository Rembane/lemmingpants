{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Types
    ( AgendaItem(..)
    , Attendee(..)
    , Config(..)
    , MessageType(..)
    ) where

import Control.Concurrent.STM.TChan (TChan)
import Data.Aeson
import Data.Monoid
import Data.Text
import Database.Selda
import Database.Selda.Backend (SeldaConnection)
import GHC.Generics hiding ((:*:))
import qualified Network.WebSockets as WS

-- | Internal stuff for message passing.
data MessageType where
    Notify    :: WS.WebSocketsData a => a -> MessageType

-- | Configuration
data Config = Config
    { seldaConn :: SeldaConnection
    , msgChan   :: TChan MessageType
    }

-- Id, CID
data Attendee = Attendee
    { id :: RowID
    , cid :: Text
    } deriving (Generic, Show)

-- This is quite boilerplaty, we'll take care of it later.
instance ToJSON Attendee where
    toJSON (Attendee id' cid) =
        object ["id" .= fromRowId id', "cid" .= cid]

    toEncoding (Attendee id' cid) =
        pairs ("id" .= fromRowId id' <> "cid" .= cid)

data AgendaItem = AgendaItem
    { id      :: RowID
    , title   :: Text
    , content :: Text
    , order   :: Int -- ^ The order of the items on the agenda.
    } deriving Generic

-- This is quite boilerplaty, we'll take care of it later.
instance ToJSON AgendaItem where
    toJSON (AgendaItem id' title content order) =
        object ["id" .= fromRowId id', "title" .= title, "content" .= content, "order" .= order]

    toEncoding (AgendaItem id' title content order) =
        pairs ("id" .= fromRowId id' <> "title" .= title <> "content" .= content <> "order" .= order)

