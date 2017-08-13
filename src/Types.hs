{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types
    ( Attendee(..)
    ) where

import Data.Aeson
import Data.Monoid
import Data.Text
import Database.Selda
import GHC.Generics hiding ((:*:))

-- Id, CID
data Attendee = Attendee
    { id :: RowID
    , cid :: Text
    } deriving Generic

-- This is quite boilerplaty, we'll take care of it later.
instance ToJSON Attendee where
    toJSON (Attendee id' cid) =
        object ["id" .= fromRowId id', "cid" .= cid]

    toEncoding (Attendee id' cid) =
        pairs ("id" .= fromRowId id' <> "cid" .= cid)
