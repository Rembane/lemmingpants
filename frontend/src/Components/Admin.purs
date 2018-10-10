module Components.Admin where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Components.SpeakerQueue as SQ
import Data.Either (Either(..), either, note)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.HTTP.Method (Method(..))
import Data.List as L
import Data.Maybe (Maybe(Nothing, Just))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Postgrest (createURL)
import Postgrest as PG
import Types.Agenda (Agenda, AgendaItem(..))
import Types.Agenda as AG
import Types.Attendee (AttendeeDB)
import Types.Flash as FL
import Types.Token (Payload(..), Token(..))
import Web.Event.Event (Event, preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

foreign import setFocusImpl :: Fn1 String (Effect Unit)

setFocus :: String -> Effect Unit
setFocus = runFn1 setFocusImpl

type State =
  { agenda      :: Agenda
  , token       :: Token
  , attendees   :: AttendeeDB
  }

type Input = State

data Query a
  = Init                   a
  | PreviousAI             a
  | NextAI                 a
  | SQMsg       SQ.Message a
  | GotNewState State      a
  | HandleKey   Event      a

derive instance functorQuery :: Functor Query

data Message
  = Flash FL.Flash

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.lifecycleParentComponent
    { initialState: identity
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: HE.input GotNewState
    }
  where
    render :: State -> H.ParentHTML Query SQ.Query Unit Aff
    render state =
      if pl.role == "admin_user"
        then
          HH.div_
            ([ HH.h1
              [HP.class_ (HH.ClassName "clearfix")]
              [ HH.button
                [ HE.onClick (HE.input_ PreviousAI), HP.id_ "prev-button" ]
                [ HH.text "⇐" ]
              , HH.text (either identity (\(AgendaItem a) -> a.title) currentAI)
              , HH.button
                [ HE.onClick (HE.input_ NextAI), HP.id_ "next-button" ]
                [ HH.text "⇒" ]
              ]
            ] <> either
                (const [HH.text "No agenda item => no speaker queue."])
                (\ai@(AgendaItem {id, speakerQueues}) ->
                  [ case AG.topSQ ai of
                      Nothing -> HH.text "We have no speaker queues I'm afraid. This shouldn't happen. It happened anyway."
                      Just sq ->
                        HH.slot
                          unit
                          SQ.component
                          { speakerQueue: sq
                          , token:        state.token
                          , attendees:    state.attendees
                          , agendaItemId: id
                          , sqHeight:     L.length speakerQueues
                          }
                          (HE.input SQMsg)
                  ])
                currentAI
            )
        else
          HH.div_
            [ HH.p_
              [ HH.text "Please "
              , HH.a
                  [ HP.href "#login" ]
                  [ HH.text "login"  ]
              , HH.text " to access the admin interface."
              ]
            ]
      where
        currentAI    = AG.getCurrentAI state.agenda
        (Payload pl) = let (Token t) = state.token in t.payload

    eval :: Query ~> H.ParentDSL State Query SQ.Query Unit Message Aff
    eval =
      case _ of
        Init next -> do
          document <- H.liftEffect $ Web.document =<< Web.window
          H.liftEffect $ setFocus "id"
          H.subscribe $ ES.eventSource
            (\f -> do
              el <- eventListener (f <<< H.action <<< HandleKey)
              addEventListener (KET.keydown) el true (HTMLDocument.toEventTarget document)
            )
            (pure <<< map (const ES.Listening))
          pure next
        PreviousAI next ->
           step AG.prev *> pure next
        NextAI next ->
          step AG.next *> pure next
        SQMsg (SQ.Flash m) next ->
          H.raise (Flash m) *> pure next
        GotNewState s next ->
          H.put s *> pure next
        HandleKey ev next ->
          let p  = const (H.liftEffect $ preventDefault ev)
              go =
                case _ of
                  "n" -> p unit *> H.query unit (H.action SQ.Next)  *> pure next -- Next speaker
                  "e" -> p unit *> H.query unit (H.action SQ.Eject) *> pure next -- Eject speaker
                  "h" -> p unit *> step AG.prev *> pure next -- Previous agenda item
                  "l" -> p unit *> step AG.next *> pure next -- Next agenda item
                  _   -> pure next
           in sequence (fromEvent ev <#> (key >>> go)) *> pure next

    step
      :: (Agenda -> Maybe Agenda)
      -> H.HalogenM State Query SQ.Query Unit Message Aff Unit
    step f =
      H.get
        >>= \{agenda} ->
            setCurrentAgendaItem ((note "ERROR: Agenda: Out of bounds step." (f agenda)) >>= AG.getCurrentAI)

    setCurrentAgendaItem
      :: Either String AgendaItem
      -> H.HalogenM State Query SQ.Query Unit Message Aff Unit
    setCurrentAgendaItem mai =
      case mai of
        Left s ->
          H.raise $ Flash $ FL.mkFlash s FL.Error
        Right (AgendaItem c) -> do
          s <- H.get
          r <- H.liftAff (PG.emptyResponse
                 (createURL "/rpc/set_current_agenda_item")
                 s.token
                 POST
                 {id: c.id})
          case r.status of
            StatusCode 200 ->
              pure unit
            _ ->
              H.raise $ Flash $
                FL.mkFlash
                  "setCurrentAgendaItem -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
                  FL.Error

