module SpeakerQueueStack where

import Types

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import DOM.Event.Event (currentTarget)
import DOM.Node.Element (getAttribute)
import DOM.Node.Types (Element, Node)
import Data.Array as A
import Data.Either (Either(..))
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Prelude (type (~>), Unit, Void, bind, const, id, pure, show, unit, ($), (*>), (<$>), (<<<))
import Simple.JSON (readJSON)
import Unsafe.Coerce (unsafeCoerce)

type State =
  { stack         :: Array SpeakerQueue
  , agendaId      :: Int
  , addAttendeeId :: Maybe Int
  }

type Input = State

data Query a
  = PushSpeakerQueue a
  | PopSpeakerQueue a
  | DequeSpeaker a
  | RemoveSpeaker Int a
  | EnqueueSpeaker a
  | SetAddAttendeeId (Maybe Int) a

type SpeakerQueueStackEffects e = (Aff (ajax :: AJAX, console :: CONSOLE | e))

nodeToElement :: Node -> Element
nodeToElement = unsafeCoerce

component :: forall e. H.Component HH.HTML Query Input Void (SpeakerQueueStackEffects e)
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.section_
        [ HH.h1_
          [ HH.text "Speakers" ]
        , HH.p_
          [ HH.text ("Current speaker: " <> maybe "---" (\(Attendee {cid}) -> cid) (getCurrent state)) ]
        , HH.button
          [ HE.onClick (HE.input_ DequeSpeaker) ]
          [ HH.text "POP!" ]
        , HH.ol_
          ( mapFlipped (getCurrent state).speakers (\s ->
              HH.li_
                [ HH.button
                  [ HE.onClick (HE.input_ (RemoveSpeaker s.id)) ]
                  [ HH.text "DEL" ]
                , HH.text (" " <> show s.cid)
                ]
            )
          )
        , HH.p_
          [ HH.text ("Speaker stack height: " <> (show (A.length (getCurrent state)))) ]
        , HH.button
          [ HE.onClick (HE.input_ PushSpeakerQueue) ]
          [ HH.text "Push speaker queue" ]
        , HH.button
          [ HE.onClick (HE.input_ PopSpeakerQueue) ]
          [ HH.text "Pop speaker queue" ]
        , HH.p_
          [ HH.label_
            [ HH.text "Attendee number"
            , HH.br_
            , HH.input
              [ HP.type_ "number"
              , HP.value (maybe "" show state.addAttendeeId)
              , HE.onChange (SetAddAttendeeId <$> getAttribute "value" <<< nodeToElement <<< currentTarget) -- TODO: Fix!
              ]
            ]
          ]
        , HH.p_
          [ HH.input
            [ HE.onClick (HE.input_ EnqueueSpeaker)
            , HP.type_ "submit"
            , HP.value "Add to speaker list"
            ]
          ]
        ]

    getCurrent :: State -> Maybe SpeakerQueue
    getCurrent s = A.index s.stack 0

    eval :: Query ~> H.ComponentDSL State Query Void (SpeakerQueueStackEffects e)
    eval =
      case _ of
        PushSpeakerQueue     next -> rpcHelper  "/agenda/speakerQueue/push"             *> pure next
        PopSpeakerQueue      next -> rpcHelper  "/agenda/speakerQueue/pop"              *> pure next
        DequeSpeaker         next -> rpcHelper  "/agenda/speaker/deque"                 *> pure next
        RemoveSpeaker id_    next -> rpcHelper ("/agenda/speaker/remove/" <> show id_)  *> pure next
        EnqueueSpeaker       next -> do
          s <- H.get
          case s.addAttendeeId of
            Nothing  -> pure next
            Just id_ -> rpcHelper ("/agenda/speaker/enqueue/" <> show id_)
              *> H.modify (_ { addAttendeeId = Nothing })
              *> pure next

        SetAddAttendeeId mid next -> H.modify (_ { addAttendeeId = mid }) *> pure next

      where
        rpcHelper :: String -> H.ComponentDSL State Query Void (SpeakerQueueStackEffects e) Unit
        rpcHelper url = do
          r <- H.liftAff $ AX.post url unit
          case readJSON r.response of
            Left es -> H.liftAff $ logShow es
            Right v -> H.modify (_ { current = v } )

