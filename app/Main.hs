module Main where

import Control.Exception (bracket)
import Database.Selda.SQLite (seldaClose, sqliteOpen)
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Lib

main :: IO ()
main = do
    bracket
        (sqliteOpen "datalemming.db")
        (seldaClose)
        (\conn -> do
            dbSetup conn
            withStdoutLogger $ \aplogger ->
                runSettings
                    (setPort 8000 $ setLogger aplogger defaultSettings)
                    (app (Config conn)))

