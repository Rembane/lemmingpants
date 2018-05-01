module Components.SpeakerQueue where

import Components.Forms as F
import Components.Forms.Field (mkField)
import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(Right, Left))
import Data.Foreign (MultipleErrors)
import Data.HTTP.Method (Method(..))
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
import Postgrest (createURL)
import Postgrest as PG
import Prelude (type (~>), Unit, bind, id, map, pure, show, unit, ($), (*>), (<=), (<>))
import Simple.JSON (readJSON)
import Types.Attendee (Attendee(..), AttendeeDB, getAttendeeByNumber)
import Types.Speaker (Speaker(..), visualizeSpeaker)
import Types.SpeakerQueue (SpeakerQueue(..))
import Types.Token (Token)

type State =
  { agendaItemId :: Int
  , speakerQueue :: SpeakerQueue
  , token        :: Token
  , attendees    :: AttendeeDB
  , sqHeight     :: Int
  }

data Query a
  = PushSQ                a
  | PopSQ                 a
  | FormMsg     F.Message a
  | Next                  a
  | Eject                 a
  | Delete      Int       a
  | GotNewState State     a

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
      HH.div
        [ HP.id_ "speakerhandling-container" ]
        [ HH.div
          [ HP.id_ "speaker-col" ]
          [ HH.p_
            [ HH.strong_ [ HH.text "Speaking: " ]
            , HH.text (maybe "–" (visualizeSpeaker state.attendees) sq.speaking)
            ]
          , HH.table
              [ HP.id_ "attendee-table" ]
              ([ HH.thead_
                [ HH.tr_
                  [ HH.th
                      [ HP.class_ (HH.ClassName "id") ]
                      [ HH.text "ID"]
                  , HH.th [ HP.id_ "name"      ] [ HH.text "Name"]
                  , HH.th
                      [ HP.class_ (HH.ClassName "numspoken")
                      , HP.title ("Number of times spoken")
                      ]
                      [ HH.text "#"]
                  , HH.th [ HP.id_ "delcol"    ] [ HH.text " "]
                  ]
                ]
              ] <>
              (map
                (\s@(Speaker s') ->
                  HH.tr_
                    [ HH.td
                      [ HP.class_ (HH.ClassName "id") ]
                      [ HH.text (show s'.attendeeId) ]
                    , HH.td_ [ HH.text (visualizeSpeaker state.attendees s) ]
                    , HH.td
                        [ HP.class_ (HH.ClassName "numspoken") ]
                        [ HH.text (show s'.timesSpoken) ]
                    , HH.td_ [ HH.button [ HE.onClick (HE.input_ (Delete s'.id)) ] [ HH.text "X" ] ]
                    ])
                sq.speakers))
          ]
        , HH.div
            [ HP.id_ "speaker-button-col" ]
            [ HH.p_ [ HH.text ("Stack height: " <> show state.sqHeight) ]
            , HH.ul_
              [ HH.li_ [ HH.button [ HE.onClick (HE.input_ PushSQ) ] [ HH.text "Push speakerqueue" ] ]
              , HH.li_
                [ HH.button
                  [ HE.onClick (HE.input_ PopSQ)
                  , if state.sqHeight <= 1
                      then HP.disabled true
                      else HP.disabled false
                  ]
                  [ HH.text "Pop speakerqueue" ]
                ]
              ]
            , HH.ul
              [ HP.class_ (HH.ClassName "with-top-margin") ]
              [ HH.li_ [ HH.button [ HE.onClick (HE.input_ Eject) ] [HH.text "Eject current speaker"] ]
              , HH.li_ [ HH.button [ HE.onClick (HE.input_ Next)  ] [HH.text "Next speaker"] ]
              ]
            , HH.slot
                unit
                (F.component "Add speaker"
                  [ mkField "id" "Speaker ID" [HP.type_ HP.InputNumber, HP.required true, HP.autocomplete false] ]
                )
                unit
                (HE.input FormMsg)
            ]
        ]
      where
        (SpeakerQueue sq) = state.speakerQueue

    eval :: Query ~> H.ParentDSL State Query F.Query Unit Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        PushSQ next -> do
          state <- H.get
          r <- H.liftAff $ PG.emptyResponse
             (createURL "/speaker_queue")
            state.token
            POST
            { agenda_item_id: state.agendaItemId
            , state:          "active" }
          case r.status of
            StatusCode 201 -> -- The `Created` HTTP status code.
              pure unit
            _ ->
              H.raise (Flash "PushSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        PopSQ next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          r <- H.liftAff $ PG.emptyResponse
             (createURL $ "/speaker_queue?id=eq." <> show sq.id)
             state.token
             PATCH
             { state: "done" }
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
                Right n -> do
                  case getAttendeeByNumber n state.attendees of
                    Nothing           ->
                      H.raise (Flash ("Couldn't find attendee with number: " <> show n))
                    Just (Attendee a) -> do
                      r <- H.liftAff $ PG.emptyResponse
                        (createURL "/speaker")
                        state.token
                        POST
                        { speaker_queue_id: sq.id
                        , attendee_id:      a.id
                        }
                      case r.status of
                        StatusCode 201 -> -- The `Created` HTTP status code.
                          pure unit
                        StatusCode 409 -> -- We can only have a visible speaker once per speaker queue.
                          H.raise (Flash "I'm sorry, but you cannot add a speaker while it still is in the speaker queue.")
                        _ ->
                          H.raise (Flash "SpeakerQueue.FormMsg -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        Next next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case A.head sq.speakers of
            Nothing          -> pure unit
            Just (Speaker s) -> do
              r <- H.liftAff $ PG.emptyResponse
                      (createURL "/rpc/set_current_speaker")
                      state.token
                      POST
                      {id: s.id}
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
              r <- H.liftAff $ PG.emptyResponse
                      (createURL $ "/speaker?id=eq." <> show s.id)
                      state.token
                      PATCH
                      { state: "done" }
              case r.status of
                StatusCode 204 -> -- The `Created` HTTP status code.
                  pure unit
                _ ->
                  H.raise (Flash "SpeakerQueue.Eject -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        Delete id_ next -> do
          state <- H.get
          r <- H.liftAff $ PG.emptyResponse
                  (createURL $ "/speaker?id=eq." <> show id_)
                  state.token
                  PATCH
                  { state: "deleted" }
          case r.status of
            StatusCode 204 -> -- The `Created` HTTP status code.
              pure unit
            _ ->
              H.raise (Flash "SpeakerQueue.Delete -- ERROR! Got a HTTP response we didn't expect! See the console for more information.")
          *> pure next
        GotNewState s next -> H.put s *> pure next
