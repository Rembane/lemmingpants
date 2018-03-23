module Main where

import Browser.WebStorage (getItem, localStorage)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Effects (LemmingPantsEffects)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lemmingpants as LP
import Prelude (Unit, bind, discard, unit, void)

main :: forall e. Eff (LemmingPantsEffects (console :: CONSOLE | e)) Unit
main = do
  log "Hello lemming!"
  token <- getItem localStorage LP.tokenKey
  HA.runHalogenAff do
    body   <- HA.awaitBody
    driver <- runUI (LP.component token) unit body
    void (liftEff (LP.routeSignal driver))
