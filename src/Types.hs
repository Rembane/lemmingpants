{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Types
    ( Agenda(..)
    , AgendaItem(..)
    , FancyAgendaItem
    , Attendee(..)
    , MessageType(..)
    , SpeakerQueue(..)

    , mkFancyAgendaItem
    ) where

import Control.Concurrent.STM.TChan (TChan)
import Data.Aeson
import Data.Aeson.Encoding.Internal ((><))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text
import Data.Sequence (Seq)
import GHC.Generics
import qualified Network.WebSockets as WS

-- | Internal stuff for message passing.
data MessageType where
  Notify               :: WS.WebSocketsData a => a -> MessageType
  NewCurrentAgendaItem :: FancyAgendaItem          -> MessageType

-- Id, CID
data Attendee = Attendee
  { id  :: Int
  , cid :: Text
  } deriving (Eq, FromJSON, Generic, Show, ToJSON)

data SpeakerQueue = SpeakerQueue
  { speakers :: Seq Attendee
  , current  :: Maybe Attendee
  } deriving (FromJSON, Generic, ToJSON)

data AgendaItem = AgendaItem
  { title             :: Text
  , content           :: Text
  , speakerQueueStack :: [SpeakerQueue]
  } deriving (FromJSON, Generic, ToJSON)

-- | This is used for frontend stuff.
-- It contains the order and the agenda item.
newtype FancyAgendaItem = FancyAgendaItem (Int, AgendaItem)

instance ToJSON FancyAgendaItem where
  toJSON (FancyAgendaItem (o, a)) = let (Object x) = toJSON a in Object (HM.insert "order" (toJSON o) x)

mkFancyAgendaItem :: Int -> AgendaItem -> FancyAgendaItem
mkFancyAgendaItem i a = FancyAgendaItem (i,a)

data Agenda = Agenda
  { history :: [AgendaItem] -- ^ The items that are done.
  , future  :: [AgendaItem] -- ^ The items that are in the future,
                            -- the first element is the current one.
  } deriving (FromJSON, Generic, ToJSON)
