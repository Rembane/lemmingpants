module Main where

import Control.Concurrent.STM.TChan (newBroadcastTChanIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (bracket)
import Network.Wai.Logger
import Network.Wai.Handler.Warp
import System.Directory

import Lib
import qualified DB

dbFilename :: String
dbFilename = "lemmingpants.db"

-- | Dies horribly if database can't be loaded.
loadOrDie :: IO DB.Database
loadOrDie = do
    exists <- doesFileExist dbFilename
    if exists
       then do
            db <- DB.load dbFilename
            case db of
              Left  s   -> error s
              Right db' -> return db'
       else
            return DB.empty

main :: IO ()
main = bracket
    ( do
        db <- loadOrDie
        v1 <- newTVarIO db
        v2 <- newBroadcastTChanIO
        return (v1, v2)
    )
    ( DB.save dbFilename . fst)
    ( \(db, bcChan) -> do
        putStrLn "\nInitializing..."
        putStrLn "Running..."
        withStdoutLogger $ \aplogger ->
            runSettings
                (setPort 8000 $ setLogger aplogger defaultSettings)
                (app (Config db bcChan))
    )
