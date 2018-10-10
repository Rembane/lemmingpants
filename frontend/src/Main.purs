module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as Res
import Control.Apply (lift3)
import Data.Bifunctor (lmap, rmap)
import Data.DateTime.Instant (instant)
import Data.Either (Either(Right, Left), either)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Data.Validation.Semigroup (V, invalid, unV)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Lemmingpants as LP
import Postgrest as PG
import Routing.Hash (matches)
import Simple.JSON (readJSON)
import Types.Agenda (Agenda)
import Types.Agenda as AG
import Types.Attendee (Attendee, newAttendeeDB)
import Types.Token (Payload(..), Token(..), loadToken, removeToken, saveToken)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent) as ME
import Web.Socket.WebSocket as WS

parseAgenda :: String -> Either MultipleErrors Agenda
parseAgenda = readJSON

parseAttendees :: String -> Either MultipleErrors (Array Attendee)
parseAttendees = readJSON

convertErrors :: forall a. Either Res.ResponseFormatError a -> Either MultipleErrors a
convertErrors = lmap (pure <<< ForeignError <<< Res.printResponseFormatError)

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

loadInitialState :: Aff (Either MultipleErrors LP.Input)
loadInitialState = do
  w <- liftEffect window
  sequential
    (combineFailures <$>                                       parallel (loadOrGetNewToken w)
                     <*> ((\{body} -> parseAgenda    =<< (convertErrors body)) <$> parallel (getString initialAgendaUrl))
                     <*> ((\{body} -> parseAttendees =<< (convertErrors body)) <$> parallel (getString initialAttendeesUrl)))
    <#> rmap (\tpl ->
      { token:                           get1 tpl
      , agenda:    AG.jumpToFirstActive (get2 tpl)
      , attendees: newAttendeeDB        (get3 tpl)
      , window:    w
      })
  where
    getString = AX.get Res.string

    initialAgendaUrl = PG.createURL "/agenda_item?select=*,speakerQueues:speaker_queue(id,state,speakers:active_speakers(id,attendeeId:attendee_id,state,timesSpoken:times_spoken))&speaker_queue.state=in.(init,active)&speaker_queue.speakers.state=in.(init,active)&order=order_.asc&speaker_queue.order=id.asc"
    initialAttendeesUrl = PG.createURL "/attendee?select=id,cid,name,nick,numbers:attendee_number(id)"

    -- | Get a new token if the current token is too old.
    -- | Otherwise, make do with the one saved in local storage.
    loadOrGetNewToken :: Window -> Aff (Either MultipleErrors Token)
    loadOrGetNewToken w = do
      et <- loadToken w
      case et of
        Left  _ -> do
          et' <- PG.requestAnonymousToken
          case et' of
            Left  l -> (pure <<< Left) l
            Right r -> do
              _ <- saveToken w r
              (pure <<< pure) r
        Right t@(Token t') ->
          (liftEffect now) >>= \n ->
            let (Payload p) = t'.payload
                mexp        = instant (Milliseconds ((toNumber p.exp) * 1000.0))
             in if maybe false (_ > n) mexp
                  then (pure <<< pure) t
                  else removeToken w *> loadOrGetNewToken w -- The recursion hack!

main :: Effect Unit
main = do
  log "Hello lemming!"
  HA.runHalogenAff do
    body <- HA.awaitBody
    est  <- loadInitialState
    case est of
      Left  es -> liftEffect $ log $ "ERROR: " <> foldMap renderForeignError es
      Right st -> do
        connection <-
          let (Token t') = st.token
           in liftEffect $ WS.create (PG.createWSURL $ "/state_updates/" <> t'.raw) []
        driver <- runUI LP.component st body
        liftEffect $ do
          sendMsg <- eventListener (\e -> launchAff_ $ fromMaybe (pure unit) (driver.query <<< H.action <<< LP.WSMsg <<< ME.data_ <$> ME.fromEvent e))
          addEventListener onMessage sendMsg false (WS.toEventTarget connection)
          void (matches LP.locations (const (launchAff_ <<< driver.query <<< H.action <<< LP.ChangePage)))

