module Effects where

import Browser.WebStorage (WEB_STORAGE)
import Halogen.Aff as HA
import Network.HTTP.Affjax (AJAX)

type LemmingPantsEffects e = HA.HalogenEffects (webStorage :: WEB_STORAGE, ajax :: AJAX | e)
