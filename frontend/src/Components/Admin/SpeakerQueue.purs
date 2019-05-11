module Components.Admin.SpeakerQueue where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Data.Array as A
import Data.HTTP.Method (Method(..))
import Data.Lens (filtered, preview, traversed, view)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import FormHelpers (FieldError, fieldScaffolding, isInt, isNonEmpty)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Postgrest (createURL)
import Postgrest as PG
import Simple.JSON (class WriteForeign)
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
  = PushSQ                     a
  | PopSQ                      a
  | Formless (F.Message' Form) a
  | Next                       a
  | Eject                      a
  | Delete      Int            a
  | GotNewState State          a

type ChildQuery = F.Query' Form Aff

data Message
  = Flash (Maybe FL.Flash)

newtype Form r f = Form (r
  ( id   :: f FieldError String Int
  ))
derive instance newtypeForm :: Newtype (Form r f) _

component :: H.Component HH.HTML Query State Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: HE.input GotNewState
    }

  where
    render :: State -> H.ParentHTML Query ChildQuery Unit Aff
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
                    (let (SpeakerQueue {speakerAdded}) = speakerQueue
                      in if Just s == speakerAdded
                           then HH.tr [ HP.class_ $ HH.ClassName "speaker-added" ]
                           else HH.tr_
                    ) [ HH.td
                        [ HP.class_ (HH.ClassName "id") ]
                        [ HH.text (show attendeeId) ]
                      , HH.td_
                        [ HH.text (S.visualizeSpeaker attendees s) ]
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
                F.component
                  { initialInputs, validators, render: renderFormless }
                  (HE.input Formless)
            ]
        ]
      where
        proxy = F.FormProxy :: F.FormProxy Form

        initialInputs :: Form Record F.InputField
        initialInputs = F.mkInputFields proxy

        validators :: Form Record (F.Validation Form Aff)
        validators = Form
          { id: isInt <<< isNonEmpty }

        renderFormless :: F.State Form Aff -> F.HTML' Form Aff
        renderFormless s =
          HH.form
            [ HE.onSubmit (HE.input_ F.submit) ]
            [ fieldScaffolding "Speaker ID"
              [ HH.input
                [ HP.value $ F.getInput r.id s.form
                , HE.onValueInput $ HE.input $ F.setValidate r.id
                , HP.type_ HP.InputNumber
                , HP.required true
                , HP.autocomplete false
                , HP.id_ "id"
                ]
              ]
            , HH.p_
              [ HH.input
                [ HP.type_ HP.InputSubmit
                , HP.value "Add speaker"
                ]
              ]
            ]
          where
            r = F.mkSProxies proxy

    eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Message Aff
    eval =
      case _ of
        PushSQ next ->
          H.raise (Flash Nothing)
          *> H.get >>= \{agendaItemId} ->
            ajaxHelper
              "/speaker_queue"
              POST
              { agenda_item_id: agendaItemId
              , state:          "active" }
              201
              "PushSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          *> pure next
        PopSQ next ->
          H.raise (Flash Nothing)
          *> H.get >>= \{speakerQueue} ->
            let (SpeakerQueue {id}) = speakerQueue
             in ajaxHelper
                  ("/speaker_queue?id=eq." <> show id)
                  PATCH
                  { state: "done" }
                  204
                  "PopSQ -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
          *> pure next
        Formless m next ->
          H.raise (Flash Nothing)
          *> H.get
          >>= \({attendees, speakerQueue, token}) ->
            case m of
              F.Submitted formOutput -> do
                let form = F.unwrapOutputFields formOutput
                case getAttendeeByNumber form.id attendees of
                  Nothing           ->
                    H.raise $ Flash $ Just $ FL.mkFlash ("Couldn't find attendee with number: " <> show form.id) FL.Error
                  Just (Attendee a) ->
                    H.liftAff (PG.emptyResponse
                      (createURL "/speaker")
                      token
                      POST
                      { speaker_queue_id: let (SpeakerQueue {id}) = speakerQueue in id
                      , attendee_id:      a.id
                      })
                    >>= \{status} ->
                      case status of
                        StatusCode 201 -> -- The `Created` HTTP status code.
                          H.query unit (H.action F.resetAll) $> unit
                        StatusCode 409 -> -- We can only have a visible speaker once per speaker queue.
                          H.raise $ Flash $ Just $ FL.mkFlash "I'm sorry, but you cannot add a speaker while it still is in the speaker queue." FL.Error
                        _ ->
                          H.raise $ Flash $ Just $ FL.mkFlash "SpeakerQueue.FormMsg -- ERROR! Got a HTTP response we didn't expect! See the console for more information." FL.Error
                pure next
              _ -> pure next
          *> pure next
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
            Nothing               -> pure unit
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
  -> H.ParentDSL State Query ChildQuery Unit Message Aff Unit
ajaxHelper partialUrl method dta code msg = do
  {token} <- H.get
  {status} <- H.liftAff $ PG.emptyResponse
          (createURL partialUrl)
          token
          method
          dta
  if status == StatusCode code
    then pure unit
    else H.raise $ Flash $ Just $ FL.mkFlash msg FL.Error
