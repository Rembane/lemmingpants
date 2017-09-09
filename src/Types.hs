{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Types
    ( AgendaItem(..)
    , Attendee(..)
    , MessageType(..)
    , SpeakerQueue(..)
    ) where

import Control.Concurrent.STM.TChan (TChan)
import Data.Aeson
import Data.Monoid
import Data.Text
import qualified Data.Vector as V
import Data.UUID (UUID, fromWords, toWords)
import GHC.Generics
import qualified Network.WebSockets as WS

-- | Internal stuff for message passing.
data MessageType where
    Notify    :: WS.WebSocketsData a => a -> MessageType

-- Id, CID
data Attendee = Attendee
    { id  :: Int
    , cid :: Text
    } deriving (FromJSON, Generic, Show, ToJSON)

newtype SpeakerQueue = SpeakerQueue
    { speakers :: V.Vector Attendee }
    deriving (FromJSON, Generic, ToJSON)

data AgendaItem = AgendaItem
    { id                :: UUID
    , title             :: Text
    , content           :: Text
    , speakerQueueStack :: [SpeakerQueue]
    , order             :: Int
    } deriving (FromJSON, Generic, ToJSON)
