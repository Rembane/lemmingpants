module Effects where

import Browser.WebStorage (WEB_STORAGE)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Halogen.Aff as HA
import Network.HTTP.Affjax (AJAX)

type LemmingPantsEffects e =
  HA.HalogenEffects (console :: CONSOLE, webStorage :: WEB_STORAGE, ajax :: AJAX, now :: NOW | e)
