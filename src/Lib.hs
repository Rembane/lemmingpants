{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app
    , Config(..)
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Catch (throwM, try)
import Control.Monad.STM
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Prelude hiding (id)
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles

import qualified DB
import Types

-- | Configuration
data Config = Config
  { db      :: TVar DB.Database
  , msgChan :: TChan MessageType
  }

newtype LemmingHandler a = LemmingHandler { runLemmingHandler :: ReaderT Config IO a }
  deriving ( Functor, Applicative, Monad, MonadReader Config )

type AttendeeAPI
  =    "create" :> ReqBody '[JSON] Text :> Post '[JSON] Int
  :<|> "list"   :>                         Get  '[JSON] [Attendee]

type SpeakerQueueAPI
  =    "push" :> Post '[JSON] [SpeakerQueue]
  :<|> "pop"  :> Post '[JSON] [SpeakerQueue]

type SpeakerAPI
  =    "enqueue" :> Capture "uid" Int :> Post '[JSON] [SpeakerQueue]
  :<|> "dequeue" :>                      Post '[JSON] [SpeakerQueue]
  :<|> "remove"  :> Capture "uid" Int :> Post '[JSON] [SpeakerQueue]

-- We do all mutations on the current agenda item.
type AgendaAPI
  =    Get '[JSON] FancyAgendaItem -- Get active agenda item
  :<|> "previous"     :> Post '[JSON] FancyAgendaItem
  :<|> "next"         :> Post '[JSON] FancyAgendaItem
  :<|> "speakerQueue" :> SpeakerQueueAPI
  :<|> "speaker"      :> SpeakerAPI

type LemmingAPI
  =    "attendee" :> AttendeeAPI
  :<|> "agenda"   :> AgendaAPI

type WithStaticFilesAPI
  =    LemmingAPI
  :<|> Raw -- Static files

attendeeT :: ServerT AttendeeAPI LemmingHandler
attendeeT = createAttendee :<|> listAttendees
  where
    createAttendee :: Text -> LemmingHandler Int
    createAttendee c = LemmingHandler $ do
      db' <- asks db
      a   <- liftIO $ atomically $ DB.createAttendee c db'
      wch <- asks msgChan
      liftIO $ atomically $ writeTChan wch (Notify (encode a))
      return $ (id :: Attendee -> Int) a

    listAttendees :: LemmingHandler [Attendee]
    listAttendees = LemmingHandler $ do
      db' <- asks db
      liftIO $ atomically $ DB.listAttendees db'

agendaT :: ServerT AgendaAPI LemmingHandler
agendaT = getAgendaItem
        :<|> previous
        :<|> next
        :<|> speakerQueueT
        :<|> speakerT
  where
    getAgendaItem :: LemmingHandler FancyAgendaItem
    getAgendaItem = LemmingHandler $ do
      db' <- asks db
      (liftIO . atomically . DB.getFancyAgendaItem) db'

    previous :: LemmingHandler FancyAgendaItem
    previous = LemmingHandler $ do
      db' <- asks db
      wch <- asks msgChan
      (liftIO . atomically . DB.previousAgendaItem) db'
      a <- (liftIO . atomically . DB.getFancyAgendaItem) db'
      (liftIO . atomically . writeTChan wch) (NewCurrentAgendaItem a)
      return a

    next :: LemmingHandler FancyAgendaItem
    next = LemmingHandler $ do
      db' <- asks db
      wch <- asks msgChan
      (liftIO . atomically . DB.nextAgendaItem) db'
      a <- (liftIO . atomically . DB.getFancyAgendaItem) db'
      (liftIO . atomically . writeTChan wch) (NewCurrentAgendaItem a)
      return a

speakerQueueT :: ServerT SpeakerQueueAPI LemmingHandler
speakerQueueT = pushSpeakerQueue :<|> popSpeakerQueue
  where
    pushSpeakerQueue :: LemmingHandler [SpeakerQueue]
    pushSpeakerQueue = LemmingHandler $ do
      db' <- asks db
      liftIO $ atomically $ DB.pushSpeakerQueue db'

    popSpeakerQueue :: LemmingHandler [SpeakerQueue]
    popSpeakerQueue = LemmingHandler $ do
      db' <- asks db
      liftIO $ atomically $ DB.popSpeakerQueue db'

-- speakerT :: ???
speakerT = enqueueSpeaker :<|> dequeueSpeaker :<|> removeSpeaker
  where
    enqueueSpeaker :: Int -> LemmingHandler [SpeakerQueue]
    enqueueSpeaker i = getOrThrowAttendee i >>= \a -> LemmingHandler $ do
      db' <- asks db
      liftIO $ atomically $ DB.enqueueSpeaker a db'

    dequeueSpeaker :: LemmingHandler [SpeakerQueue]
    dequeueSpeaker = LemmingHandler $ do
      db' <- asks db
      liftIO $ atomically $ DB.dequeueSpeaker db'

    removeSpeaker :: Int -> LemmingHandler [SpeakerQueue]
    removeSpeaker i = getOrThrowAttendee i >>= \a -> LemmingHandler $ do
      db' <- asks db
      (liftIO . atomically) (DB.removeSpeaker a db')

getOrThrowAttendee :: Int -> LemmingHandler Attendee
getOrThrowAttendee i = LemmingHandler $ do
  db' <- asks db
  ma  <- (liftIO . atomically) (DB.getAttendee i db')
  case ma of
    Just  a -> return a
    Nothing -> throwM (err404 { errBody = "There is no attendee with this id: " <> BL.pack (show i) })

lemmingServerT :: ServerT LemmingAPI LemmingHandler
lemmingServerT = attendeeT :<|> agendaT

withStaticFilesAPI :: Proxy WithStaticFilesAPI
withStaticFilesAPI = Proxy

handleWS :: Config -> WS.PendingConnection -> IO ()
handleWS conf req = do
  conn <- WS.acceptRequest req
  WS.forkPingThread conn 30
  as <- liftIO $ atomically $ DB.listAttendees $ db conf
  WS.sendTextData conn (encode as)
  ch <- atomically $ dupTChan (msgChan conf)
  forever $ do
    msg <- atomically $ readTChan ch
    case msg of
      Notify m -> WS.sendTextData conn m

app :: Config -> Application
app conf =
  websocketsOr
    WS.defaultConnectionOptions
    (handleWS conf)
    (serve
      withStaticFilesAPI
      ((convert conf `enter` lemmingServerT) :<|> serveDirectoryFileServer "static")
    )
  where
    convert :: Config -> LemmingHandler :~> Handler
    convert c = NT (Handler . ExceptT . try . (`runReaderT` c) . runLemmingHandler)

