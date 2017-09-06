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
import Data.UUID (UUID)
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

type AgendaAPI
  =    Get '[JSON] [AgendaItem] -- List agenda
  :<|> (Capture "aid" UUID :>
            (Get '[JSON] AgendaItem -- Detail agenda item
       :<|> "speakerQueue" :> SpeakerQueueAPI
       :<|> "speaker"      :> SpeakerAPI
            )
       )

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
agendaT = listAgendaItems :<|> (\u -> getAgendaItem u :<|> speakerQueueT u :<|> speakerT u)
    where
        listAgendaItems :: LemmingHandler [AgendaItem]
        listAgendaItems = LemmingHandler $ do
            db' <- asks db
            liftIO $ atomically $ DB.listAgendaItems db'

        getAgendaItem :: UUID -> LemmingHandler AgendaItem
        getAgendaItem uuid = LemmingHandler $ do
            db' <- asks db
            r   <- liftIO $ atomically $ DB.getAgendaItem uuid db'
            case r of
              Just r' -> return r'
              Nothing -> throwM (err404 { errBody = "There is no item on the agenda with uuid: " <> BL.pack (show uuid) })


speakerQueueT :: UUID -> ServerT SpeakerQueueAPI LemmingHandler
speakerQueueT uuid = pushSpeakerQueue uuid :<|> popSpeakerQueue uuid
    where
        pushSpeakerQueue :: UUID -> LemmingHandler [SpeakerQueue]
        pushSpeakerQueue uuid = LemmingHandler $ do
            db' <- asks db
            r   <- liftIO $ atomically $ DB.pushSpeakerQueue uuid db'
            case r of
              Just r' -> return (speakerQueueStack r')
              Nothing -> throwM (err404 { errBody = "There is no item on the agenda uuid: " <> BL.pack (show uuid) <> " doesn't exist!" })

        popSpeakerQueue :: UUID -> LemmingHandler [SpeakerQueue]
        popSpeakerQueue uuid = LemmingHandler $ do
            db' <- asks db
            r   <- liftIO $ atomically $ DB.popSpeakerQueue uuid db'
            case r of
              Just r' -> return (speakerQueueStack r')
              Nothing -> throwM (err404 { errBody = "There is no item on the agenda uuid: " <> BL.pack (show uuid) <> " doesn't exist!" })

-- speakerT :: ???
speakerT uuid = enqueueSpeaker uuid :<|> dequeueSpeaker uuid :<|> removeSpeaker uuid
    where
        enqueueSpeaker :: UUID -> Int -> LemmingHandler [SpeakerQueue]
        enqueueSpeaker uuid i = LemmingHandler $ do
            db' <- asks db
            r   <- liftIO $ atomically $ DB.enqueueSpeaker i uuid db'
            case r of
              Just r' -> return (speakerQueueStack r')
              Nothing -> throwM (err404 { errBody = "There is no item on the agenda uuid: " <> BL.pack (show uuid) <> " doesn't exist!" })

        dequeueSpeaker :: UUID -> LemmingHandler [SpeakerQueue]
        dequeueSpeaker uuid = LemmingHandler $ do
            db' <- asks db
            r   <- liftIO $ atomically $ DB.dequeueSpeaker uuid db'
            case r of
              Just r' -> return (speakerQueueStack r')
              Nothing -> throwM (err404 { errBody = "There is no item on the agenda uuid: " <> BL.pack (show uuid) <> " doesn't exist!" })

        removeSpeaker :: UUID -> Int -> LemmingHandler [SpeakerQueue]
        removeSpeaker uuid i = LemmingHandler $ do
            db' <- asks db
            r   <- liftIO $ atomically $ DB.removeSpeaker i uuid db'
            case r of
              Just r' -> return (speakerQueueStack r')
              Nothing -> throwM (err404 { errBody = "There is no item on the agenda uuid: " <> BL.pack (show uuid) <> " doesn't exist!" })

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
app conf = websocketsOr
            WS.defaultConnectionOptions
            (handleWS conf)
            (serve
                withStaticFilesAPI
                ((convert conf `enter` lemmingServerT) :<|> serveDirectoryFileServer "static")
            )
    where
        convert :: Config -> LemmingHandler :~> Handler
        convert c = NT (Handler . ExceptT . try . (`runReaderT` c) . runLemmingHandler)

