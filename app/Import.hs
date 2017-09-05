{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.STM (atomically)
import Data.List (zip3)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import System.Environment (getArgs)
import Text.Megaparsec

import qualified DB

dbFilename :: String
dbFilename = "lemmingpants.db"

-- | Matches a line with a \punkt{...} and cleans up everything
--   after the last bracket until newline.
punkt :: Parsec () String String
punkt = string "\\punkt{" *> someTill anyChar (char '}') <* manyTill anyChar eol

-- | Devours a line and returns empty string.
devourLine :: Parsec () String String
devourLine = manyTill anyChar eol *> return ""

main :: IO ()
main = do
    fn <- (!! 0) <$> getArgs
    contents <- (fmap (filter (/= "")) . parse (many (punkt <|> devourLine)) fn) <$> readFile fn
    case contents of
      Left  e  -> putStrLn ("Got this weird error while parsing file. Stopping now." <> show e)
      Right vs -> bracket
                    ( do
                        db <- DB.loadOrDie dbFilename
                        newTVarIO db
                    )
                    ( DB.save dbFilename )
                    ( \db -> do
                        putStrLn "Running with database..."
                        forM_ (zip [1..] vs) $ \(o, v) -> do
                            uuid <- nextRandom
                            atomically (DB.createAgendaItem uuid o (T.pack v) "" db)
                    )

