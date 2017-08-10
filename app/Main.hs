module Main where

import Database.Selda.SQLite
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Lib

main :: IO ()
main = do
    conn <- sqliteOpen "datalemming.db"
    dbSetup conn
    withStdoutLogger $ \aplogger ->
        runSettings
            (setPort 8000 $ setLogger aplogger defaultSettings)
            (app (Config conn))
