{-# LANGUAGE DataKinds #-}
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
    , DB.dbSetup
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Catch (throwM, try)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text
import Database.Selda.Backend
import Database.Selda.Unsafe (unsafeRowId)
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles

import qualified DB as DB
import Types

data Config = Config
    { seldaConn :: SeldaConnection
    }

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
            i <- runSeldaT (DB.createAttendee c) conn
            case i of
              Just i' -> return i'
              Nothing   ->
                  throwM (err409 { errBody = "An attendee with this CID already exists, and it cannot be created again. Talk to the Talhennapresidiet if you have any questions on how to solve this situation." })

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

app :: Config -> Application
app c = serve withStaticFilesAPI (((convert c) `enter` lemmingServerT) :<|> serveDirectoryFileServer "static")
    where
        convert :: Config -> LemmingHandler :~> Handler
        convert c = NT (Handler . ExceptT . try . (`runReaderT` c) . runLemmingHandler)

