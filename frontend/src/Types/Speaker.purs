module Types.Speaker where

import Data.Newtype (class Newtype)
import Data.Record.ShowRecord (showRecord)
import Prelude (class Eq, class Ord, class Show, compare, (<>))
import Simple.JSON (class ReadForeign)

newtype Speaker = Speaker
  { id          :: Int
  , attendeeId  :: Int
  , state       :: String
  , timesSpoken :: Int
  }
derive instance         ntSp :: Newtype     Speaker _
derive newtype instance rfSp :: ReadForeign Speaker
derive instance         eqSp :: Eq          Speaker

instance shSp :: Show Speaker where
  show (Speaker s) = "Speaker " <> showRecord s

instance ordSp :: Ord Speaker where
  compare (Speaker s1) (Speaker s2) =
    compare s1.timesSpoken s2.timesSpoken <> compare s1.id s2.id
