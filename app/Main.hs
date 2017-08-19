module Main where

import Control.Concurrent.STM.TChan (newBroadcastTChanIO)
import Control.Exception (bracket)
import Database.Selda.SQLite (seldaClose, sqliteOpen)
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Lib

main :: IO ()
main = bracket
    ( (,) <$> sqliteOpen "datalemming.db"
          <*> newBroadcastTChanIO
    )
    ( seldaClose . fst )
    ( \(db, bcChan) -> do
        putStrLn "\nInitializing..."
        putStrLn "Running..."
        dbSetup db
        withStdoutLogger $ \aplogger ->
            runSettings
                (setPort 8000 $ setLogger aplogger defaultSettings)
                (app (Config db bcChan))
    )
