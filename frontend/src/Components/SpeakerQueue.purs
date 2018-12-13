module Components.SpeakerQueue where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Components.Forms as F
import Components.Forms.Field (mkField)
import Data.Array as A
import Data.Either (Either(Right, Left))
import Data.HTTP.Method (Method(..))
import Data.Lens (filtered, preview, traversed, view)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Effect.Aff (Aff)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Postgrest (createURL)
import Postgrest as PG
import Simple.JSON (class WriteForeign, readJSON)
import Types.Attendee (Attendee(..), AttendeeDB, getAttendeeByNumber)
import Types.Flash as FL
import Types.Speaker as S
import Types.SpeakerQueue (SpeakerQueue(..), _Speakers, _Speaking)
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
  = Flash (Maybe FL.Flash)

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
    render {attendees, speakerQueue, sqHeight} =
      HH.div
        [ HP.id_ "speakerhandling-container" ]
        [ HH.div
          [ HP.id_ "speaker-col" ]
          [ HH.p_
            [ HH.strong_ [ HH.text "Speaking: " ]
            , HH.text
                (maybe
                  "–"
                  (S.visualizeSpeaker attendees)
                  (preview (_Speakers <<< _Speaking) speakerQueue))
            ]
          , HH.table
              [ HP.id_ "attendee-table" ]
              [ HH.thead_
                [ HH.tr_
                  [ HH.th
                      [ HP.class_ (HH.ClassName "id") ]
                      [ HH.text "ID" ]
                  , HH.th [ HP.id_ "name" ] [ HH.text "Name" ]
                  , HH.th
                      [ HP.class_ (HH.ClassName "numspoken")
                      , HP.title ("Number of times spoken")
                      ]
                      [ HH.text "#" ]
                  , HH.th [ HP.id_ "delcol" ] [ HH.text " " ]
                  ]
                ]
              , HH.tbody_
                (A.fromFoldable (map
                  (\s@(S.Speaker {attendeeId, id, timesSpoken}) ->
                    HH.tr_
                      [ HH.td
                        [ HP.class_ (HH.ClassName "id") ]
                        [ HH.text (show attendeeId) ]
                      , HH.td_
                        [ HH.text (S.visualizeSpeaker attendees s)
                        , HH.div [ HP.class_ $ HH.ClassName "newCircle" ] []
                        ]
                      , HH.td
                          [ HP.class_ (HH.ClassName "numspoken") ]
                          [ HH.text (show timesSpoken) ]
                      , HH.td_ [ HH.button [ HE.onClick (HE.input_ (Delete id)) ] [ HH.text "X" ] ]
                      ])
                  (A.dropWhile (\(S.Speaker {state}) -> state == S.Active) $ view _Speakers speakerQueue)))
              ]
          ]
        , HH.div
            [ HP.id_ "speaker-button-col" ]
            [ HH.p_ [ HH.text ("Stack height: " <> show sqHeight) ]
            , HH.ul_
              [ HH.li_ [ HH.button [ HE.onClick (HE.input_ PushSQ) ] [ HH.text "Push speakerqueue" ] ]
              , HH.li_
                [ HH.button
                  [ HE.onClick (HE.input_ PopSQ)
                  , if sqHeight <= 1
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
                  [ mkField "id" "Speaker ID"
                    [HP.type_ HP.InputNumber
                    , HP.required true
                    , HP.autocomplete false
                    , HP.id_ "id"
                    ]
                  ]
                )
                unit
                (HE.input FormMsg)
            ]
        ]

    eval :: Query ~> H.ParentDSL State Query F.Query Unit Message Aff
    eval =
      case _ of
        PushSQ next -> do
          H.raise (Flash Nothing)
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
          H.raise (Flash Nothing)
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
          H.raise (Flash Nothing)
          let (SpeakerQueue sq) = state.speakerQueue
          case m of
            F.FormSubmitted m' ->
              case readJSON (unsafePartial (fromJust (FO.lookup "id" m'))) :: Either MultipleErrors Int of
                Left es -> H.liftEffect $ log $ show es
                Right n -> do
                  case getAttendeeByNumber n state.attendees of
                    Nothing           ->
                      H.raise $ Flash $ Just $ FL.mkFlash ("Couldn't find attendee with number: " <> show n) FL.Error
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
                          H.query unit (H.action F.ClearForm) *> pure unit
                        StatusCode 409 -> -- We can only have a visible speaker once per speaker queue.
                          H.raise $ Flash $ Just $ FL.mkFlash "I'm sorry, but you cannot add a speaker while it still is in the speaker queue." FL.Error
                        _ ->
                          H.raise $ Flash $ Just $ FL.mkFlash "SpeakerQueue.FormMsg -- ERROR! Got a HTTP response we didn't expect! See the console for more information." FL.Error
          pure next
        Next next -> do
          H.raise (Flash Nothing)
          {speakerQueue} <- H.get
          case preview (_Speakers <<< traversed <<< filtered (\(S.Speaker {state}) -> state /= S.Active)) speakerQueue of
            Nothing               -> pure unit
            Just (S.Speaker {id}) -> do
              ajaxHelper
                "/rpc/set_current_speaker"
                POST
                { id: id }
                200
                "SpeakerQueue.Next -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          pure next
        Eject next -> do
          H.raise (Flash Nothing)
          {speakerQueue} <- H.get
          case preview (_Speakers <<< _Speaking) speakerQueue of
            Nothing            -> pure unit
            Just (S.Speaker {id}) -> do
              ajaxHelper
                ("/speaker?id=eq." <> show id)
                PATCH
                { state: "done" }
                204
                "SpeakerQueue.Eject -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          pure next
        Delete id_ next -> do
          H.raise (Flash Nothing)
          ajaxHelper
            ("/speaker?id=eq." <> show id_)
            PATCH
            { state: "deleted" }
            204
            "SpeakerQueue.Delete -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          pure next
        GotNewState s next ->
          H.put s *> pure next

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
  {token} <- H.get
  r <- H.liftAff $ PG.emptyResponse
          (createURL partialUrl)
          token
          method
          dta
  if r.status == StatusCode code
    then pure unit
    else H.raise $ Flash $ Just $ FL.mkFlash msg FL.Error
