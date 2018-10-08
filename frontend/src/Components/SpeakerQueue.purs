module Components.SpeakerQueue where

import Prelude 

import Components.Forms as F
import Components.Forms.Field (mkField)
import Data.Array as A
import Data.Either (Either(Right, Left))
import Data.HTTP.Method (Method(..))
import Data.Lens (filtered, preview, traversed, view)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Foreign (MultipleErrors)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Postgrest (createURL)
import Postgrest as PG
import Simple.JSON (class WriteForeign, readJSON)
import Types.Attendee (Attendee(..), AttendeeDB, getAttendeeByNumber)
import Types.Flash as FL
import Types.Speaker as S
import Types.SpeakerQueue (SpeakerQueue(..), _Speakers, _Speaking)
import Types.Token (Token)
import Types.KPUpdates
import Web.Event.Event (Event)

foreign import myEvent :: Event

type State =
  { agendaItemId :: Int
  , speakerQueue :: SpeakerQueue
  , token        :: Token
  , attendees    :: AttendeeDB
  , sqHeight     :: Int
  , keyboardMsg  :: Maybe KPUpdates
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
  = Flash FL.Flash

component :: H.Component HH.HTML Query State Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: HE.input GotNewState
    }

  where
    render :: State -> H.ParentHTML Query F.Query Unit Aff
    render state =
      HH.div
        [ HP.id_ "speakerhandling-container" ]
        [ HH.div
          [ HP.id_ "speaker-col" ]
          [ HH.p_
            [ HH.strong_ [ HH.text "Speaking: " ]
            , HH.text (maybe "–" (S.visualizeSpeaker state.attendees) (preview (_Speakers <<< _Speaking) state.speakerQueue))
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
              (A.fromFoldable (map
                (\s@(S.Speaker s') ->
                  HH.tr_
                    [ HH.td
                      [ HP.class_ (HH.ClassName "id") ]
                      [ HH.text (show s'.attendeeId) ]
                    , HH.td_ [ HH.text (S.visualizeSpeaker state.attendees s) ]
                    , HH.td
                        [ HP.class_ (HH.ClassName "numspoken") ]
                        [ HH.text (show s'.timesSpoken) ]
                    , HH.td_ [ HH.button [ HE.onClick (HE.input_ (Delete s'.id)) ] [ HH.text "X" ] ]
                    ])
                (A.dropWhile (\(S.Speaker s) -> s.state == S.Active) $ view _Speakers state.speakerQueue))))
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

    eval :: Query ~> H.ParentDSL State Query F.Query Unit Message Aff
    eval =
      case _ of
        PushSQ next -> do
          state <- H.get
          ajaxHelper
            "/speaker_queue"
            POST
            { agenda_item_id: state.agendaItemId
            , state:          "active" }
            201
            "PushSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          *> pure next
        PopSQ next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          ajaxHelper
            ("/speaker_queue?id=eq." <> show sq.id)
            PATCH
            { state: "done" }
            204
            "PopSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          *> pure next
        FormMsg m next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case m of
            F.FormSubmitted m' ->
              case readJSON (unsafePartial (fromJust (FO.lookup "id" m'))) :: Either MultipleErrors Int of
                Left es -> traceM es
                Right n -> do
                  case getAttendeeByNumber n state.attendees of
                    Nothing           ->
                      H.raise $ Flash $ FL.mkFlash ("Couldn't find attendee with number: " <> show n) FL.Error
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
                          H.raise $ Flash $ FL.mkFlash "I'm sorry, but you cannot add a speaker while it still is in the speaker queue." FL.Error
                        _ ->
                          H.raise $ Flash $ FL.mkFlash "SpeakerQueue.FormMsg -- ERROR! Got a HTTP response we didn't expect! See the console for more information." FL.Error
          *> pure next
        Next next -> do
          state <- H.get
          case preview (_Speakers <<< traversed <<< filtered (\(S.Speaker s) -> s.state /= S.Active)) state.speakerQueue of
            Nothing            -> pure unit
            Just (S.Speaker s) -> do
              ajaxHelper
                "/rpc/set_current_speaker"
                POST
                { id: s.id }
                200
                "SpeakerQueue.Next -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          pure next
        Eject next -> do
          state <- H.get
          let (SpeakerQueue sq) = state.speakerQueue
          case preview (_Speakers <<< _Speaking) state.speakerQueue of
            Nothing            -> pure unit
            Just (S.Speaker s) -> do
              ajaxHelper
                ("/speaker?id=eq." <> show s.id)
                PATCH
                { state: "done" }
                204
                "SpeakerQueue.Eject -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          pure next
        Delete id_ next -> do
          ajaxHelper
            ("/speaker?id=eq." <> show id_)
            PATCH
            { state: "deleted" }
            204
            "SpeakerQueue.Delete -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          pure next
        GotNewState s next -> case s.keyboardMsg of
          Nothing   -> H.put s *> pure next
          Just kmsg -> case kmsg of 
            NextSpeaker       -> H.put (s { keyboardMsg = Nothing }) *> eval (H.action Next)  *> pure next
            EjectSpeaker      -> H.put (s { keyboardMsg = Nothing }) *> eval (H.action Eject) *> pure next
            AddSpeakerToQueue -> H.put (s { keyboardMsg = Nothing }) *> H.query unit (H.action (F.SubmitForm myEvent)) *> pure next
            _                 -> pure next



  -- = NextSpeaker
  -- | EjectSpeaker
  -- | AddSpeakerToQueue

ajaxHelper
  :: forall r
   . WriteForeign r
  => String
  -> Method
  -> r
  -> Int
  -> String
  -> H.ParentDSL State Query F.Query Unit Message Aff Unit
ajaxHelper partialUrl method dta code msg = do
  state <- H.get
  r <- H.liftAff $ PG.emptyResponse
          (createURL partialUrl)
          state.token
          method
          dta
  if r.status == StatusCode code
    then pure unit
    else H.raise $ Flash $ FL.mkFlash msg FL.Error
