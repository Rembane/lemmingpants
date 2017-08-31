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
import Data.Serialize (Serialize(..))
import Data.Serialize.Text ()
import Data.UUID (UUID, fromWords, toWords)
import Data.Vector.Serialize ()
import GHC.Generics
import qualified Network.WebSockets as WS

-- | Internal stuff for message passing.
data MessageType where
    Notify    :: WS.WebSocketsData a => a -> MessageType

-- Id, CID
data Attendee = Attendee
    { id  :: Int
    , cid :: Text
    } deriving (Generic, Show, Serialize, ToJSON)

newtype SpeakerQueue = SpeakerQueue
    { speakers :: V.Vector Attendee }
    deriving (Generic, Serialize, ToJSON)

data AgendaItem = AgendaItem
    { id                :: UUID
    , title             :: Text
    , content           :: Text
    , speakerQueueStack :: [SpeakerQueue]
    , order             :: Int
    } deriving (Generic, Serialize, ToJSON)

instance Serialize UUID where
    put = put . toWords
    get = (\(a,b,c,d) -> fromWords a b c d) <$> get
