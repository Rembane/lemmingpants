{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( Punkt
  , Underpunkt
  , kallelseParser
  , insertionQuery
  , punktToSQL
  ) where

import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Database.PostgreSQL.Simple
import Text.Megaparsec
import Text.Megaparsec.Char

-- You can remove this and import Data.Functor when base reaches version 4.11.0.0
(<&>) = flip fmap

newtype Underpunkt = Underpunkt Text
  deriving Show
data Punkt = Punkt Text [Underpunkt]
  deriving Show

type CustomError = Void
type Parser = Parsec CustomError Text
type PError = ParseError Char CustomError

kallelseParser :: Parser [Punkt]
kallelseParser
  =   skipManyTill
        anyChar
        (string "\\begin{foredragningslista}" *> space)
  *>  manyTill
        (many commentParser *> punktParser)
        (string "\\end{foredragningslista}" *> takeRest)

punktParser :: Parser Punkt
punktParser
  =   between
        (string "\\punkt{")
        (char '}')
        (many (notChar '}'))
  >>= \t ->
      space
      *>  many commentParser
      *>  optional (string "\\elab" *> skipUntilEOL *> space)
      *>  optional (string "\\secr" *> skipUntilEOL *> space)
      *>  optional ((string "\\begin{underpunkter}" <|> string "\\begin{itemize}")
          *> space *> underpunktParser)
      <&> (Punkt (T.pack t) . fromMaybe [])
      <*  space

underpunktParser :: Parser [Underpunkt]
underpunktParser
  =   optional (string "\\setlength" *> skipUntilEOL *> space)
  *>  optional (string "\\end{underpunkter}" <|> string "\\end{itemize}")
  >>=
    maybe
      ((string "\\item " *> untilCommentOrEOL <&> (Underpunkt . T.strip . T.pack) <* space)
        >>= \p -> (p:) <$> underpunktParser)
      (const $ pure [])

commentParser :: Parser ()
commentParser = char '%' *> skipUntilEOL *> space

skipUntilEOL :: Parser ()
skipUntilEOL = skipMany (satisfy (\c -> c /= '\r' && c /= '\n'))

-- | We take characters until we find a comment or EOL,
--   and then we skip until EOL, saving EOL.
untilCommentOrEOL :: Parser String
untilCommentOrEOL
  = manyTill
      anyChar
      ((char '%' *> skipUntilEOL) <|> void eol <|> string "\\elab" *> skipUntilEOL)

-- | Some SQL shenanigans
insertionQuery :: Query
insertionQuery = "INSERT INTO agenda_item(supertitle, title) VALUES(?, ?)"

punktToSQL :: Punkt -> [(Maybe Text, Text)]
punktToSQL (Punkt t us)
  = (Nothing, t) : map (\(Underpunkt u) -> (Just t, u)) us

