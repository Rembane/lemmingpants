{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB
    ( attendee
    , createAttendee
    , dbSetup
    ) where

import Control.Error (hush)
import Control.Monad.Catch (MonadCatch, tryJust)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.SQLite

attendee :: Table (RowID :*: Text)
attendee =   table "attendee"
         $   autoPrimary "id"
         :*: unique (required "cid")

-- | Creates and attendee with CID: cid' and returns the ID the attendee got wrapped in a Maybe.
-- If the attendee already exists, the operation returns Nothing.
createAttendee :: (MonadCatch m, MonadIO m) => Text -> SeldaT m (Maybe Int)
createAttendee cid' = do
    result <- errorHandling (insert_ attendee [ def :*: cid' ])
    case result of
      Just _  -> Just . fromRowId . head <$> query (getId cid')
      Nothing -> return Nothing

   where
       getId :: Text -> Query s (Col s RowID)
       getId cid' = do
           (id :*: cid) <- select attendee
           restrict (cid .== text cid')
           return id

       errorHandling :: MonadCatch m => m a -> m (Maybe a)
       errorHandling = fmap hush . tryJust err
           where
               err e@(SqlError _) = Just e
               err e              = Nothing

dbSetup :: SeldaConnection -> IO ()
dbSetup s = (flip runSeldaT) s $ do
    tryCreateTable attendee
