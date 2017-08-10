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

import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Catch (throwM, try)
import Data.Proxy (Proxy(..))
import Data.Text
import Database.Selda.Backend
import Servant.API
import Servant.Server

import qualified DB as DB

data Config = Config
    { seldaConn :: SeldaConnection
    }

newtype LemmingHandler a = LemmingHandler { runLemmingHandler :: ReaderT Config IO a }
    deriving ( Functor, Applicative, Monad, MonadReader Config )

type LemmingAPI = "attendee" :> "create" :> ReqBody '[JSON] Text :> Post '[JSON] Int

lemmingAPI :: Proxy LemmingAPI
lemmingAPI = Proxy

convert :: Config -> LemmingHandler :~> Handler
convert c = NT (Handler . ExceptT . try . (`runReaderT` c) . runLemmingHandler)

lemmingServerT :: ServerT LemmingAPI LemmingHandler
lemmingServerT = createAttendee
    where
        createAttendee :: Text -> LemmingHandler Int
        createAttendee cid = LemmingHandler $ do
            conn <- asks seldaConn
            liftIO (print cid)
            id' <- runSeldaT (DB.createAttendee cid) conn
            case id' of
              Just id'' -> return id''
              Nothing   ->
                  throwM (err409 { errBody = "An attendee with this CID already exists, and it cannot be created again. Talk to the Talhennapresidiet if you have any questions on how to solve this situation." })

app :: Config -> Application
app c = serve lemmingAPI ((convert c) `enter` lemmingServerT)
