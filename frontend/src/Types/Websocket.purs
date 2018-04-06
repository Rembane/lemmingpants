module Types.Websocket where

import Data.Foreign (Foreign)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign)

-- | The types of the messages we get over websocket.

{-
data WSMessageType
  = AgendaItemInsert
  | AgendaItemUpdate
  | AttendeeInsert
  | AttendeeUpdate
  | SpeakerInsert
  | SpeakerUpdate
  | SpeakerQueueInsert
  | SpeakerQueueUpdate

instance rfWMT :: ReadForeign WSMessageType where
  readImpl fr
    =   "agenda_item_insert" <$> read fr
    <|> "agenda_item_update" <$> read fr
    <|> "attendee_insert" <$> read fr
    <|> "attendee_update" <$> read fr
    <|> "speaker_insert" <$> read fr
    <|> "speaker_update" <$> read fr
    <|> "speaker_queue_insert" <$> read fr
    <|> "speaker_queue_update" <$> read fr
    -}
type WSMsg =
  { channel :: String
  , event   :: String
  , payload :: Foreign
  }

newtype AgendaItemMessage = AgendaItemMessage
  { id      :: Int
  , title   :: String
  , content :: String
  , order_  :: Int
  , state   :: String
  }
derive instance         ntAIM :: Newtype     AgendaItemMessage _
derive newtype instance rfAIM :: ReadForeign AgendaItemMessage

newtype SpeakerMessage = SpeakerMessage
  { id             :: Int
  , speakerQueueId :: Int
  , attendeeId     :: Int
  , state          :: String
  , timesSpoken    :: Int
  }
derive instance         ntSM :: Newtype     SpeakerMessage _
derive newtype instance rfSM :: ReadForeign SpeakerMessage

newtype SpeakerQueueMessage = SpeakerQueueMessage
  { id           :: Int
  , agendaItemId :: Int
  , state        :: String
  }
derive instance         ntSQM :: Newtype     SpeakerQueueMessage _
derive newtype instance rfSQM :: ReadForeign SpeakerQueueMessage

newtype AttendeeMessage = AttendeeMessage
  { id      :: Int
  , cid     :: String
  , name    :: String
  , nick    :: String
  , created :: String
  }
derive instance         ntAM :: Newtype     AttendeeMessage _
derive newtype instance rfAM :: ReadForeign AttendeeMessage
