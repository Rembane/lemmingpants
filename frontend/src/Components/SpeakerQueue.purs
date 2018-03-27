module Components.SpeakerQueue where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.HTTP.Method (Method(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Record.ShowRecord (showRecord)
import Data.Sequence.Ordered as OrdSeq
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Debug.Trace (traceA, traceAnyA)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, map, pure, show, unit, ($), (*>), (<$>), (<>), (==))
import Simple.JSON (read, readJSON)
import Types (Attendee(Attendee), Speaker(Speaker), SpeakerQueue(..))

type State =
  { agendaItemId :: Int
  , speakerQueue :: SpeakerQueue
  , token        :: Maybe String
  , attendees    :: M.Map Int Attendee
  }

data Query a
  = PushSQ            a
  | PopSQ             a
  | FormMsg F.Message a
  | Next              a
  | Eject             a
  | GotNewState State a

data Message
  = Flash       String
  | Pop
  | Push        SpeakerQueue
  | ModifiedTop SpeakerQueue -- ^ The new SpeakerQueue

component :: forall e. H.Component HH.HTML Query State Message (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState: moveActive
    , render
    , eval
    , receiver: HE.input GotNewState
    }

  where
    render :: State -> H.ParentHTML Query F.Query Unit (Aff (LemmingPantsEffects e))
    render state =
      HH.div_
        [ HH.div_
          [ HH.button [ HE.onClick (HE.input_ PushSQ) ] [ HH.text "Push speakerqueue" ]
          , HH.button [ HE.onClick (HE.input_ PopSQ)  ] [ HH.text "Pop speakerqueue" ]
          ]
        , HH.div_
            [ HH.p_
              [ HH.strong_ [HH.text "Speaking: "]
              , HH.text (maybe "–" (visualizeSpeaker state) sq.speaking)
              ]
            , HH.ol_
                (map
                  (\s -> HH.li_ [HH.text (visualizeSpeaker state s) ])
                  (OrdSeq.toUnfoldable sq.speakers))
            , HH.slot
              unit
              (F.component "Add speaker"
                [ mkField "id" "ID" [HP.type_ HP.InputNumber, HP.required true] ]
              )
              unit
              (HE.input FormMsg)
            , HH.ul_
                [ HH.li_ [ HH.button [ HE.onClick (HE.input_ Eject) ] [HH.text "Eject current speaker"] ]
                , HH.li_ [ HH.button [ HE.onClick (HE.input_ Next)  ] [HH.text "Next speaker"] ]
                ]
            ]
        ]
      where
        (SpeakerQueue sq) = state.speakerQueue

    visualizeSpeaker :: State -> Speaker -> String
    visualizeSpeaker state (Speaker s) =
      case M.lookup s.attendeeId state.attendees of
        Nothing           -> "ERROR: Not found!"
        Just (Attendee a) -> fromMaybe a.name a.nick

    eval :: Query ~> H.ParentDSL State Query F.Query Unit Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        PushSQ next -> do
          state <- H.get
          er <- H.liftAff (PG.signedInAjax
             "http://localhost:3000/speaker_queue?select=*,speakers:active_speakers(id,attendeeId:attendee_id,state,timesSpoken:times_spoken)"
            state.token
            POST
            [ RequestHeader "Prefer" "return=representation"
            , RequestHeader "Accept" "application/vnd.pgrst.object+json"
            ]
            { agenda_item_id: state.agendaItemId
            , state:          "active" })
          case er of
            Left  es -> H.raise (Flash es)
            Right r  ->
              case r.status of
                -- The Location-header contains the new Attendee URL.
                StatusCode 201 -> -- The `Created` HTTP status code.
                  case readJSON r.response of
                    Left es ->
                      H.raise (Flash (foldMap renderForeignError es))
                    Right sq ->
                      H.raise (Push sq)
                _ ->
                  H.raise (Flash "PushSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
                  *> H.liftAff (log (showRecord r))
          *> pure next
        PopSQ next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          er <- H.liftAff (PG.signedInAjax
             ("http://localhost:3000/speaker_queue?id=eq." <> show sq.id)
             state.token
             PATCH
             mempty
             { state: "done" })
          case er of
            Left es -> H.raise (Flash es)
            Right r ->
              case r.status of
                StatusCode 204 -> -- No Content
                  H.raise (Flash "Speaker queue popped.")
                  *> H.raise Pop
                _ ->
                  traceAnyA r *> traceA r.response
          *> pure next
        FormMsg m next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case m of
            F.FormSubmitted m' -> do
              er <- H.liftAff $ PG.signedInAjax
                "http://localhost:3000/speaker?select=id"
                state.token
                POST
                [ RequestHeader "Prefer" "return=representation"
                , RequestHeader "Accept" "application/vnd.pgrst.object+json"
                ]
                { speaker_queue_id: sq.id
                , attendee_id:      unsafePartial (fromJust (SM.lookup "id" m'))
                }
              case er of
                Left  es -> traceAnyA es
                Right r1 ->
                  case r1.status of
                    -- The Location-header contains the new Attendee URL.
                    StatusCode 201 -> -- The `Created` HTTP status code.
                      case (readJSON r1.response) :: Either MultipleErrors ({id :: Int}) of
                        Left  es   -> traceA (foldMap renderForeignError es)
                        Right {id} -> do
                          as <- (\r -> parseActiveSpeakers r.response) <$>
                            H.liftAff (AX.get (activeSpeakersUrl <> "&id=eq." <> show id))
                          case as of
                            Left  es  -> traceA (foldMap renderForeignError es)
                            Right as' -> H.raise (ModifiedTop (SpeakerQueue (sq { speakers = OrdSeq.merge sq.speakers (OrdSeq.fromFoldable as') })))
                    _ ->
                      traceAnyA r1 *> traceA r1.response
          *> pure next
        Next next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case OrdSeq.popLeast sq.speakers of
            Nothing                     -> pure unit
            Just (Tuple (Speaker s) ss) -> do
              er <- H.liftAff $ PG.signedInAjax
                      "http://localhost:3000/rpc/set_current_speaker"
                      state.token
                      POST
                      mempty
                      {id: s.id}
              case er of
                Left  m -> pure unit
                Right r -> do
                  case parseInt r.response of
                    Left es -> traceA (foldMap renderForeignError es)
                    Right 0 -> traceA "ERROR: Next: Got 0 back. Cannot set current speaker."
                    Right i ->
                      if i == s.id
                        then H.raise (ModifiedTop (SpeakerQueue (sq { speaking = Just (Speaker s), speakers = ss })))
                        else traceA "ERROR: Next: The id of the speaker we sent and the one we got back differ."
          *> pure next
        Eject next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case sq.speaking of
            Nothing          -> pure unit
            Just (Speaker s) -> do
              er <- H.liftAff $ PG.signedInAjax
                      ("http://localhost:3000/speaker?id=eq." <> show s.id)
                      state.token
                      PATCH
                      mempty
                      { state: "done" }
              case er of
                Left es -> pure unit
                Right r ->
                  case r.status of
                    StatusCode 204 -> -- No Content
                      H.raise (ModifiedTop (SpeakerQueue (sq { speaking = Nothing })))
                    _ -> traceAnyA r *> traceA r.response
          *> pure next
        GotNewState s next -> H.put (moveActive s) *> pure next

    parseInt :: Foreign -> Either MultipleErrors Int
    parseInt = read

    parseActiveSpeakers :: Foreign -> Either MultipleErrors (Array Speaker)
    parseActiveSpeakers = read

    activeSpeakersUrl :: String
    activeSpeakersUrl = "http://localhost:3000/active_speakers?select=id,attendeeId:attendee_id,state,timesSpoken:times_spoken"

    -- | If there is an active speaker, move it from the queue to the speaking slot.
    moveActive :: State -> State
    moveActive state =
      case sq.speaking of
        Just _  -> state
        Nothing ->
          case OrdSeq.popLeast sq.speakers of
            Nothing                      -> state
            Just (Tuple (Speaker s) sq') ->
              if s.state == "active"
                then state { speakerQueue = SpeakerQueue (sq { speaking = Just (Speaker s), speakers = sq' }) }
                else state
      where
        (SpeakerQueue sq) = state.speakerQueue
