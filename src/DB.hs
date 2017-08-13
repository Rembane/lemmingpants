{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB
    ( attendee
    , createAttendee
    , listAttendees
    , agendaItem
    , getAgendaItem
    , listAgendaItems
    , dbSetup
    ) where

import Control.Error (hush)
import Control.Monad.Catch (MonadCatch, tryJust)
import Data.Maybe (listToMaybe)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic
import Prelude hiding (id)

import Types

attendee :: GenTable Attendee
attendee = genTable "attendee" [(id :: Attendee -> RowID) :- autoPrimaryGen, cid :- uniqueGen]

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

-- --------------------------------------------------------------------------

agendaItem :: GenTable AgendaItem
agendaItem = genTable "agendaItem" [(id :: (AgendaItem -> RowID)) :- autoPrimaryGen, title :- uniqueGen]

-- | Gets one agenda item with the corresponding id or Nothing if it doesn't correspond.
getAgendaItem :: (MonadCatch m, MonadIO m) => RowID -> SeldaT m (Maybe AgendaItem)
getAgendaItem i' = do
    as' <- query $ do
        as@(i :*: _) <- select (gen agendaItem)
        restrict (literal i' .== i)
        return as
    return $ listToMaybe $ fromRels $ as'

-- | Gets all agenda items ordered by order.
listAgendaItems :: (MonadCatch m, MonadIO m) => SeldaT m [AgendaItem]
listAgendaItems = do
    as' <- query $ do
        as@(_ :*: _ :*: _ :*: o) <- select (gen agendaItem)
        Database.Selda.order o ascending
        return as
    return $ fromRels as'

-- --------------------------------------------------------------------------

dbSetup :: SeldaConnection -> IO ()
dbSetup s = (flip runSeldaT) s $ do
    tryCreateTable (gen attendee)
    tryCreateTable (gen agendaItem)
