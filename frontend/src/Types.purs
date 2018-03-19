module Types where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype SpeakerQueue = SpeakerQueue
  { speakers :: Array Attendee
  , current  :: Maybe Attendee
  }

derive instance ntSQ :: Newtype SpeakerQueue _
derive newtype instance rfSQ :: ReadForeign SpeakerQueue

newtype Attendee = Attendee
  { id  :: Int
  , cid :: String
  }

derive instance ntAt :: Newtype Attendee _
derive newtype instance rfAt :: ReadForeign Attendee

newtype AgendaItem = AgendaItem
  { id      :: Int
  , title   :: String
  , content :: String
  , order_  :: Int
  , active  :: Boolean
  }
derive         instance ntAI :: Newtype AgendaItem _
derive newtype instance rfAI :: ReadForeign AgendaItem
derive newtype instance wfAI :: WriteForeign AgendaItem
