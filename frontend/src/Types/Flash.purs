module Types.Flash
  ( Flash(..)
  , FlashType(..)
  , mkFlash
  ) where

import Prelude (class Show)

data FlashType
  = Error
  | Info

instance ftShow :: Show FlashType where
  show Error = "error"
  show Info  = "info"

newtype Flash = Flash
  { msg :: String
  , typ :: FlashType
  }

mkFlash :: String -> FlashType -> Flash
mkFlash msg typ = Flash { msg, typ }
