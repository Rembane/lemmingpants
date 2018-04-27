{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import Data.Functor
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import System.Environment (getArgs)
import System.IO (stdin)
import Text.Megaparsec (runParser)

import Lib (insertionQuery, kallelseParser, punktToSQL)

main :: IO ()
main = do
  fn <- head <$> getArgs
  r  <- runParser kallelseParser fn . decodeUtf8 <$> B.readFile fn
  case r of
    Left  e  -> putStrLn "ERROR!" *> print e
    Right ps ->
      connectPostgreSQL "postgres://lemmingpants:lemmingpants@localhost/lemmingpants"
        >>= \c ->
          withTransaction c
            (execute_ c "SET SCHEMA 'api'"
              *> executeMany c insertionQuery (concatMap punktToSQL ps))
        >>= print
