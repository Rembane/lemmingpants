{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB
    ( attendee
    , createAttendee
    , listAttendees
    , dbSetup
    ) where

import Control.Error (hush)
import Control.Monad.Catch (MonadCatch, tryJust)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic
import Prelude hiding (id)

import Types

attendee :: GenTable Attendee
attendee = genTable "attendee" [id :- autoPrimaryGen, cid :- uniqueGen]

-- | Creates and attendee with CID: cid' and returns the ID the attendee got wrapped in a Maybe.
-- If the attendee already exists, the operation returns Nothing.
createAttendee :: (MonadCatch m, MonadIO m) => Text -> SeldaT m (Maybe Int)
createAttendee cid' = do
    result <- errorHandling (insert_ (gen attendee) [ def :*: cid' ])
    case result of
      Just _  -> Just . fromRowId . head <$> query (getId cid')
      Nothing -> return Nothing

   where
       getId :: Text -> Query s (Col s RowID)
       getId cid2 = do
           (i :*: c) <- select (gen attendee)
           restrict (c .== text cid2)
           return i

       errorHandling :: MonadCatch m => m a -> m (Maybe a)
       errorHandling = fmap hush . tryJust err
           where
               err e@(SqlError _) = Just e
               err _              = Nothing

listAttendees :: (MonadCatch m, MonadIO m) => SeldaT m [Attendee]
listAttendees = fromRels <$> query (select (gen attendee))

dbSetup :: SeldaConnection -> IO ()
dbSetup s = (flip runSeldaT) s $ do
    tryCreateTable (gen attendee)
