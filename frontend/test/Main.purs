module Test.Main where

import Data.Array as A
import Data.Either (Either(..))
import Data.Lens (over)
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, discard, identity, otherwise, show, unit, ($), (&&), (<), (<=), (<>), (==), (||))
import Simple.JSON (readJSON)
import Test.QuickCheck (Result(Success, Failed))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Types.Agenda (empty, getCurrentAI, jumpToFirstActive)
import Types.Speaker as S
import Types.SpeakerQueue (SpeakerQueue(SpeakerQueue), _Speakers)

isSuccessful :: Result -> Boolean
isSuccessful Success = true
isSuccessful _       = false

eitherToResult :: Either String Unit -> Result
eitherToResult
  = case _ of
      Left e  -> Failed e
      Right _ -> Success

resultAsEither :: Result -> (Either String Unit -> Either String Unit) -> Result
resultAsEither Success    f = eitherToResult $ f $ Right unit
resultAsEither (Failed e) f = eitherToResult $ f $ Left  e

speakersAreSortedCorrectly :: Array S.Speaker -> Result
speakersAreSortedCorrectly [] = Success
speakersAreSortedCorrectly ss
  = case A.head $ A.dropWhile isSuccessful $ A.zipWith checkPair ss' $ unsafePartial $ fromJust $ A.tail ss' of
      Nothing -> Success  -- No errors in the array.
      Just e  -> e        -- The first error in the array.
  where
    ss' = A.sort ss

    checkPair :: S.Speaker -> S.Speaker -> Result
    checkPair (S.Speaker s1) (S.Speaker s2)
      | s1.timesSpoken <  s2.timesSpoken                   = Success
      | s1.timesSpoken == s2.timesSpoken && s1.id <= s2.id = Success
      | otherwise = Failed ("Speakers are not correctly sorted for: " <> show (S.Speaker s1) <> ", " <> show (S.Speaker s2))

theSpeakersLensRespectsInvariants :: SpeakerQueue -> Result
theSpeakersLensRespectsInvariants sq@(SpeakerQueue sqr)
  = let sq'@(SpeakerQueue sqr') = over _Speakers identity sq
     in case A.head sqr.speakers of
          Just s@(S.Speaker sr) ->
            let ss' = A.filter (\(S.Speaker s1) -> s1.state == S.Deleted || s1.state == S.Done) sqr'.speakers
             in if A.null ss'
                  then Success
                  else Failed ("The final array contains speakers that are either Deleted or Done: " <> show ss')

          Nothing -> Success

main :: Effect Unit
main = run [consoleReporter] do
  describe "getCurrentAI" do
    it "doesn't crash if the Agenda is empty" $ do
      shouldEqual
        (getCurrentAI empty)
        (Left "ERROR: There is no current agenda item.")
  describe "jumpToFirstActive" do
    it "does nothing if the Agenda is empty" $ do
      shouldEqual
        (jumpToFirstActive empty)
        empty
  describe "Speakers" do
    they "are sorted correctly" $ quickCheck speakersAreSortedCorrectly
  describe "SpeakerQueue" do
    it "has a _Speakers lens that respects the invariants" $ quickCheck theSpeakersLensRespectsInvariants
    it "has a ReadForeign that behaves well on no speakers." $ do
      shouldEqual
        (readJSON  """{"id":1,"state":"init","speakers":[]}""")
        (Right $ SpeakerQueue {id: 1, state: "init", speakers: []})

  where
    they = it
