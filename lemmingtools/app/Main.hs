{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import Data.Functor
import Data.Monoid
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Options.Applicative
import System.Environment (getArgs)
import System.IO (stdin)
import Text.Megaparsec (runParser)

import Lib (insertionQuery, kallelseParser, punktToSQL)

data Options = Options
  { optDBURL :: B.ByteString
  , fn       :: [Char]
  }

optParser :: Parser Options
optParser
  = Options
  <$> option auto
    ( long "dburl"
    <> short 'u'
    <> help "The database URL"
    <> metavar "DBURL"
    <> value "postgres://lemmingpants:lemmingpants@localhost/lemmingpants"
    )
  <*> argument str
    ( metavar "FILE"
    <> help "The file to parse, preferably a tex-file, otherwise we will have a tough time parsing it."
    )

main :: IO ()
main = do
  cfg <- execParser $ info (helper <*> optParser)
                        ( fullDesc
                        <> progDesc "This is Lemmingtools, the tool suite for Lemmingpants."
                        <> header "Lemmingtools"
                        )

  r  <- runParser kallelseParser (fn cfg) . decodeUtf8 <$> B.readFile (fn cfg)
  case r of
    Left  e  -> putStrLn "ERROR!" *> print e
    Right ps ->
      connectPostgreSQL (optDBURL cfg)
        >>= \c ->
          withTransaction c
            (execute_ c "SET SCHEMA 'model'"
              *> execute_ c "SET ROLE 'admin_user'"
              *> executeMany c insertionQuery (concatMap punktToSQL ps))
        >>= print
