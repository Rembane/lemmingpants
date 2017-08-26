{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( app
    , Config(..)
    , DB.dbSetup
    ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Catch (throwM, try)
import Control.Monad.STM
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text
import Database.Selda (RowID, fromRowId)
import Database.Selda.Backend
import Database.Selda.Unsafe (unsafeRowId)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles

import qualified DB as DB
import Types

newtype LemmingHandler a = LemmingHandler { runLemmingHandler :: ReaderT Config IO a }
    deriving ( Functor, Applicative, Monad, MonadReader Config )

type LemmingAPI
  =
      ( "attendee" :>
          (    "create" :> ReqBody '[JSON] Text :> Post '[JSON] Int
          :<|> "list"   :>                         Get  '[JSON] [Attendee]
          )
      )
  :<|>
      ( "agendaitem" :>
          (    "get"  :> ReqBody '[JSON] Int :> Get '[JSON] AgendaItem
          :<|> "list" :>                        Get '[JSON] [AgendaItem]
          )
      )

lemmingServerT :: ServerT LemmingAPI LemmingHandler
lemmingServerT = (createAttendee :<|> listAttendees) :<|> (getAgendaItem :<|> listAgendaItems)
    where
        createAttendee :: Text -> LemmingHandler Int
        createAttendee c = LemmingHandler $ do
            conn <- asks seldaConn
            liftIO (print c)
            a <- runSeldaT (DB.createAttendee c) conn
            wch <- asks msgChan
            liftIO $ atomically $ writeTChan wch (Notify (encode a))
            return (fromRowId $ (Types.id :: Attendee -> RowID) a)

        listAttendees :: LemmingHandler [Attendee]
        listAttendees = LemmingHandler $ do
            conn <- asks seldaConn
            runSeldaT (DB.listAttendees) conn

        getAgendaItem :: Int -> LemmingHandler AgendaItem
        getAgendaItem i = LemmingHandler $ do
            conn <- asks seldaConn
            r <- runSeldaT (DB.getAgendaItem (unsafeRowId i)) conn
            case r of
              Just r' -> return r'
              Nothing -> throwM (err404 { errBody = "An agenda item with id: " <> BL.pack (show i) <> " doesn't exist!" })

        listAgendaItems :: LemmingHandler [AgendaItem]
        listAgendaItems = LemmingHandler $ do
            conn <- asks seldaConn
            runSeldaT (DB.listAgendaItems) conn

type WithStaticFilesAPI
  =    LemmingAPI
  :<|> Raw -- Static files

lemmingAPI :: Proxy LemmingAPI
lemmingAPI = Proxy

withStaticFilesAPI :: Proxy WithStaticFilesAPI
withStaticFilesAPI = Proxy

handleWS :: Config -> WS.PendingConnection -> IO ()
handleWS conf req = do
    conn <- WS.acceptRequest req
    WS.forkPingThread conn 30
    as <- runSeldaT (DB.listAttendees) (seldaConn conf)
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
                (((convert conf) `enter` lemmingServerT) :<|> serveDirectoryFileServer "static")
            )
    where
        convert :: Config -> LemmingHandler :~> Handler
        convert c = NT (Handler . ExceptT . try . (`runReaderT` c) . runLemmingHandler)

