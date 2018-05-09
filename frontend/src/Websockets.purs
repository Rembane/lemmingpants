module Websockets where

-- The code here is heavily inspired by:
-- https://github.com/slamdata/purescript-halogen/blob/master/examples/driver-websockets/src/Main.purs

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Maybe (Maybe(..))
import Prelude (Unit, const, (<<<))

-- TODO: Handle onError and onClose too!
-- https://pursuit.purescript.org/packages/purescript-dom/4.16.0/docs/DOM.Websocket.Event.EventTypes

wsProducer
  :: forall e
   . WS.WebSocket
  -> CR.Producer String (Aff (avar :: AVAR, exception :: EXCEPTION, dom :: DOM | e)) Unit
wsProducer socket = CRA.produce \emit ->
  EET.addEventListener
    WSET.onMessage
    (listener emit)
    false
    (WS.socketToEventTarget socket)
  where
    listener emit = EET.eventListener \e ->
      for_ (readHelper WS.readMessageEvent e) \msgEvent ->
        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
          emit (Left msg)

    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< toForeign
