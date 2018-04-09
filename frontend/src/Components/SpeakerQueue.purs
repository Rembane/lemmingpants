module Components.SpeakerQueue where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(Right, Left))
import Data.Foreign (MultipleErrors)
import Data.HTTP.Method (Method(..))
import Data.Map as M
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.StrMap as SM
import Debug.Trace (traceAnyA)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, id, map, pure, show, unit, ($), (*>), (<>))
import Simple.JSON (readJSON)
import Types.Attendee (Attendee)
import Types.Speaker (Speaker(..), visualizeSpeaker)
import Types.SpeakerQueue (SpeakerQueue(..))

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
  = Flash String

component :: forall e. H.Component HH.HTML Query State Message (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState: id
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
              , HH.text (maybe "–" (visualizeSpeaker state.attendees) sq.speaking)
              ]
            , HH.ol_
                (map
                  (\s -> HH.li_ [HH.text (visualizeSpeaker state.attendees s) ])
                  sq.speakers)
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

    eval :: Query ~> H.ParentDSL State Query F.Query Unit Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        PushSQ next -> do
          state <- H.get
          er <- H.liftAff $ PG.emptyResponse
             "http://localhost:3000/speaker_queue"
            state.token
            POST
            { agenda_item_id: state.agendaItemId
            , state:          "active" }
          case er of
            Left  es -> H.raise (Flash es)
            Right r  ->
              case r.status of
                StatusCode 201 -> -- The `Created` HTTP status code.
                  pure unit
                _ ->
                  H.raise (Flash "PushSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        PopSQ next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          er <- H.liftAff $ PG.emptyResponse
             ("http://localhost:3000/speaker_queue?id=eq." <> show sq.id)
             state.token
             PATCH
             { state: "done" }
          case er of
            Left es -> H.raise (Flash es)
            Right r ->
              case r.status of
                StatusCode 204 -> -- No Content
                  pure unit
                _ ->
                  H.raise (Flash "PopSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        FormMsg m next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case m of
            F.FormSubmitted m' ->
              case readJSON (unsafePartial (fromJust (SM.lookup "id" m'))) :: Either MultipleErrors Int of
                Left es -> traceAnyA es
                Right i -> do
                  er <- H.liftAff $ PG.emptyResponse
                    "http://localhost:3000/speaker"
                    state.token
                    POST
                    { speaker_queue_id: sq.id
                    , attendee_id:      i
                    }
                  case er of
                    Left  es -> traceAnyA es
                    Right r ->
                      case r.status of
                        StatusCode 201 -> -- The `Created` HTTP status code.
                          pure unit
                        _ ->
                          H.raise (Flash "SpeakerQueue.FormMsg -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        Next next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case A.head sq.speakers of
            Nothing          -> pure unit
            Just (Speaker s) -> do
              er <- H.liftAff $ PG.emptyResponse
                      "http://localhost:3000/rpc/set_current_speaker"
                      state.token
                      POST
                      {id: s.id}
              case er of
                Left  es -> traceAnyA es
                Right r ->
                  case r.status of
                    StatusCode 200 ->
                      pure unit
                    _ ->
                      H.raise (Flash "SpeakerQueue.Next -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        Eject next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case sq.speaking of
            Nothing          -> pure unit
            Just (Speaker s) -> do
              er <- H.liftAff $ PG.emptyResponse
                      ("http://localhost:3000/speaker?id=eq." <> show s.id)
                      state.token
                      PATCH
                      { state: "done" }
              case er of
                Left  es -> traceAnyA es
                Right r ->
                  case r.status of
                    StatusCode 204 -> -- The `Created` HTTP status code.
                      pure unit
                    _ ->
                      H.raise (Flash "SpeakerQueue.Eject -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        GotNewState s next -> H.put s *> pure next

    activeSpeakersUrl :: String
    activeSpeakersUrl = "http://localhost:3000/active_speakers?select=id,attendeeId:attendee_id,state,timesSpoken:times_spoken"

