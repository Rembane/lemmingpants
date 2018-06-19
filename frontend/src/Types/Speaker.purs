module Types.Speaker
  ( Speaker(..)
  , SpeakerState(..)
  , visualizeSpeaker
  ) where

import Control.Monad.Except (throwError)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..))
import Prelude (class Eq, class Ord, class Show, compare, pure, show, ($), (<>), (>>=))
import Simple.JSON (class ReadForeign, readImpl)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Types.Attendee (AttendeeDB, getAttendeeById, visualizeAttendee)

data SpeakerState
  = Init
  | Active
  | Done
  | Deleted

derive instance eqSS  :: Eq      SpeakerState
derive instance genSS :: Generic SpeakerState _

instance arbSS :: Arbitrary SpeakerState where
  arbitrary = genericArbitrary

instance shSS :: Show SpeakerState where
  show = case _ of
           Init    -> "init"
           Active  -> "active"
           Done    -> "done"
           Deleted -> "deleted"

instance rfSS :: ReadForeign SpeakerState where
  readImpl fr
    =   readImpl fr
    >>= case _ of
          "init"    -> pure Init
          "active"  -> pure Active
          "done"    -> pure Done
          "deleted" -> pure Deleted
          e         -> throwError $ pure $ ForeignError $ "This is not a SpeakerState: " <> e

newtype Speaker = Speaker
  { id          :: Int
  , attendeeId  :: Int
  , state       :: SpeakerState
  , timesSpoken :: Int
  }
derive instance         ntSp  :: Newtype     Speaker _
derive newtype instance rfSp  :: ReadForeign Speaker
derive instance         eqSp  :: Eq          Speaker
derive instance         genSQ :: Generic     Speaker _

instance arbSQ :: Arbitrary Speaker where
  arbitrary = genericArbitrary

instance shSp :: Show Speaker where
  show (Speaker s) = "Speaker " <> show s

instance ordSp :: Ord Speaker where
  compare (Speaker s1) (Speaker s2) =
    compare s1.timesSpoken s2.timesSpoken <> compare s1.id s2.id

visualizeSpeaker :: AttendeeDB -> Speaker -> String
visualizeSpeaker db (Speaker s) =
  maybe "ERROR: Speaker not found!" visualizeAttendee (getAttendeeById s.attendeeId db)
