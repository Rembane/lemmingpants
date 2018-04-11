module Main where

import Browser.WebStorage (getItem, localStorage)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, liftEff', parallel, sequential)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (throw)
import Coroutines (consumerToQuery, routeProducer)
import DOM.Websocket.WebSocket as WS
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceA)
import Effects (LemmingPantsEffects)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lemmingpants as LP
import Network.HTTP.Affjax as AX
import Postgrest (createURL, createWSURL)
import Postgrest as PG
import Prelude (class Semigroup, Unit, bind, discard, map, pure, void, ($), (*>), (<$>), (<*>), (<<<), (<>))
import Simple.JSON (read)
import Types.Agenda (Agenda)
import Types.Agenda as AG
import Types.Attendee (Attendee, newAttendeeDB)
import Websockets (wsProducer)

parseAgenda :: Foreign -> Either MultipleErrors Agenda
parseAgenda = read

parseAttendees :: Foreign -> Either MultipleErrors (Array Attendee)
parseAttendees = read

combineFailures :: forall a b c. Semigroup a => Either a b -> Either a c -> Either a (Tuple b c)
combineFailures (Left l1)  (Left l2)  = Left (l1 <> l2)
combineFailures (Left l1)  _          = Left l1
combineFailures _          (Left l2)  = Left l2
combineFailures (Right r1) (Right r2) = Right (Tuple r1 r2)

loadInitialState :: forall e. Aff (LemmingPantsEffects e) LP.Input
loadInitialState = do
  token <- liftEff' (getItem localStorage LP.tokenKey)
  rs    <- sequential
    (combineFailures <$> ((\r -> parseAgenda    r.response) <$> parallel (AX.get initialAgendaUrl))
                     <*> ((\r -> parseAttendees r.response) <$> parallel (AX.get initialAttendeesUrl)))
  case rs of
    Left  es             -> traceA (foldMap renderForeignError es) *> liftEff' (throw "FETCH DATA ERROR!")
    Right (Tuple ag ats) ->
      let attendees = newAttendeeDB ats
          agenda    = AG.jumpToFirstActive ag
       in pure { token, agenda, attendees }
  where
    initialAgendaUrl = createURL "/agenda_item?select=*,speakerQueues:speaker_queue(id,state,speakers:active_speakers(id,attendeeId:attendee_id,state,timesSpoken:times_spoken))&speaker_queue.state=in.(init,active)&order=order_.asc&order=speaker_queue.id.asc&speaker_queue.active_speakers.state=in.(init,active)"
    initialAttendeesUrl = createURL "/attendee?select=id,cid,name,nick,numbers:attendee_number(id)"

main :: forall e. Eff (LemmingPantsEffects (console :: CONSOLE | e)) Unit
main = do
  log "Hello lemming!"
  HA.runHalogenAff do
    body <- HA.awaitBody
    st   <- loadInitialState
    t    <- if isJust st.token
              then pure st.token
              else getToken
    case t of
      Nothing -> liftEff' (log "Couldn't get a token! :(")
      Just t' -> do
        connection <- liftEff $ WS.create (WS.URL $ createWSURL $ "/state_updates/" <> t') []
        driver     <- runUI LP.component (st { token = Just t' }) body
        pfest (wsProducer connection)      (consumerToQuery driver.query LP.WSMsg)
        pfest (routeProducer LP.locations) (consumerToQuery driver.query LP.ChangePage)
  where
    parseToken :: Foreign -> Either MultipleErrors (Array { token :: String })
    parseToken = read

    getToken :: Aff (LemmingPantsEffects (console :: CONSOLE | e)) (Maybe String)
    getToken = do
      r <- PG.post (createURL "/rpc/get_token") ""
      case parseToken r.response of
        Left  es -> logShow (foldMap renderForeignError es) *> pure Nothing
        Right ts -> pure (map (\t -> t.token) (A.head ts))

pfest
  :: forall a e
   . CR.Producer a (Aff (LemmingPantsEffects e)) Unit
  -> CR.Consumer a (Aff (LemmingPantsEffects e)) Unit
  -> Aff           (LemmingPantsEffects e) Unit
pfest a = void <<< forkAff <<< CR.runProcess <<< CR.connect a
