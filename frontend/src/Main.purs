module Main where

import Control.Apply (lift3)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, liftEff', parallel, sequential)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (now)
import Coroutines (consumerToQuery, routeProducer)
import DOM.Websocket.WebSocket as WS
import Data.Bifunctor (rmap)
import Data.DateTime.Instant (instant)
import Data.Either (Either(Right, Left), either)
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Data.Validation.Semigroup (V, invalid, unV)
import Effects (LemmingPantsEffects)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lemmingpants as LP
import Network.HTTP.Affjax as AX
import Postgrest as PG
import Prelude (type (~>), Unit, bind, discard, pure, void, ($), (*), (*>), (<#>), (<$>), (<*>), (<<<), (<>), (>), (>>=))
import Simple.JSON (read)
import Types.Agenda (Agenda)
import Types.Agenda as AG
import Types.Attendee (Attendee, newAttendeeDB)
import Types.Token (Payload(..), Token(..), loadToken, removeToken, saveToken)
import Websockets (wsProducer)

parseAgenda :: Foreign -> Either MultipleErrors Agenda
parseAgenda = read

parseAttendees :: Foreign -> Either MultipleErrors (Array Attendee)
parseAttendees = read

combineFailures
  :: forall a b c
   . Either MultipleErrors a
  -> Either MultipleErrors b
  -> Either MultipleErrors c
  -> Either MultipleErrors (Tuple3 a b c)
combineFailures e1 e2 e3 =
  let
    toV :: Either MultipleErrors ~> V MultipleErrors
    toV = either invalid pure
    fromV = unV Left Right
  in fromV (lift3 tuple3 (toV e1) (toV e2) (toV e3))

loadInitialState :: forall e. Aff (LemmingPantsEffects e) (Either MultipleErrors LP.Input)
loadInitialState = do
  sequential
    (combineFailures <$>                                        parallel loadOrGetNewToken
                     <*> ((\r -> parseAgenda    r.response) <$> parallel (AX.get initialAgendaUrl))
                     <*> ((\r -> parseAttendees r.response) <$> parallel (AX.get initialAttendeesUrl)))
    <#> rmap (\tpl ->
      { token:                           get1 tpl
      , agenda:    AG.jumpToFirstActive (get2 tpl)
      , attendees: newAttendeeDB        (get3 tpl)
      })
  where
    initialAgendaUrl = PG.createURL "/agenda_item?select=*,speakerQueues:speaker_queue(id,state,speakers:active_speakers(id,attendeeId:attendee_id,state,timesSpoken:times_spoken))&speaker_queue.state=in.(init,active)&order=order_.asc&order=speaker_queue.id.asc&speaker_queue.active_speakers.state=in.(init,active)"
    initialAttendeesUrl = PG.createURL "/attendee?select=id,cid,name,nick,numbers:attendee_number(id)"

    -- | Get a new token if the current token is too old.
    -- | Otherwise, make do with the one saved in local storage.
    loadOrGetNewToken :: Aff (LemmingPantsEffects e) (Either MultipleErrors Token)
    loadOrGetNewToken = do
      et <- loadToken
      case et of
        Left  _ -> do
          et' <- PG.requestAnonymousToken
          case et' of
            Left  l -> (pure <<< Left) l
            Right r -> do
              _ <- saveToken r
              (pure <<< pure) r
        Right t@(Token t') ->
          (liftEff' now) >>= \n ->
            let (Payload p) = t'.payload
                mexp        = instant (Milliseconds ((toNumber p.exp) * 1000.0))
             in if maybe false (_ > n) mexp
                  then (pure <<< pure) t
                  else removeToken *> loadOrGetNewToken -- The recursion hack!

pfest
  :: forall a e
   . CR.Producer a (Aff (LemmingPantsEffects e)) Unit
  -> CR.Consumer a (Aff (LemmingPantsEffects e)) Unit
  -> Aff           (LemmingPantsEffects e) Unit
pfest a = void <<< forkAff <<< CR.runProcess <<< CR.connect a

main :: forall e. Eff (LemmingPantsEffects (console :: CONSOLE | e)) Unit
main = do
  log "Hello lemming!"
  HA.runHalogenAff do
    body <- HA.awaitBody
    est  <- loadInitialState
    case est of
      Left  es -> liftEff $ log $ "ERROR: " <> foldMap renderForeignError es
      Right st -> do
        connection <-
          let (Token t') = st.token
           in liftEff $ WS.create (WS.URL $ PG.createWSURL $ "/state_updates/" <> t'.raw) []
        driver     <- runUI LP.component st body
        pfest (wsProducer connection)      (consumerToQuery driver.query LP.WSMsg)
        pfest (routeProducer LP.locations) (consumerToQuery driver.query LP.ChangePage)

