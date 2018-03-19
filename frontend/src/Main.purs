module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lemmingpants as LP
import Network.HTTP.Affjax as AX
import Prelude (Unit, bind, discard, unit, void)

main :: forall e. Eff (HA.HalogenEffects (ajax :: AX.AJAX, console :: CONSOLE, dom :: DOM | e)) Unit
main = do
  log "Hello lemming!"
  HA.runHalogenAff do
    body   <- HA.awaitBody
    driver <- runUI LP.component unit body
    void (liftEff (LP.routeSignal driver))

{-

let ais = readJSON r.response :: Either MultipleErrors (Array AgendaItem)
                  in logShow $ map writeJSON ais
                  -}
