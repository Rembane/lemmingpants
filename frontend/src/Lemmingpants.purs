module Lemmingpants where

import Browser.WebStorage (localStorage, setItem)
import Components.Admin as CA
import Components.Home as CH
import Components.Login as CL
import Components.Overhead as CO
import Components.Registration as CR
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..), note)
import Data.Either.Nested (Either5)
import Data.Foldable (foldMap)
import Data.Foreign (F, Foreign, ForeignError(..), fail, renderForeignError)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Map as M
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Monoid (mempty)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, const, pure, unit, ($), (*>), (<#>), (<$), (<>), (=<<), (==), (>>=))
import Routing.Match (Match)
import Routing.Match.Class (lit)
import Simple.JSON (readImpl, readJSON')
import Types.Agenda (Agenda, AgendaItem(AgendaItem))
import Types.Agenda as AG
import Types.Attendee (Attendee(..))
import Types.Speaker (Speaker(..))
import Types.SpeakerQueue (SpeakerQueue(..), addSpeaker, modifySpeaker)

type ChildQuery = Coproduct5 CR.Query CO.Query CA.Query CL.Query CH.Query
type ChildSlot  = Either5 Int Int Int Int Int

data WSAction
  = Insert
  | Update

data Location
  = Home
  | Registration
  | Overhead
  | Admin
  | Login

locations :: Match Location
locations
  =   (Registration <$ lit "registration")
  <|> (Overhead     <$ lit "overhead")
  <|> (Admin        <$ lit "admin")
  <|> (Login        <$ lit "login")
  <|> (Home         <$ lit "")

type State =
  { currentLocation :: Location
  , token           :: Maybe String
  , flash           :: Maybe String
  , agenda          :: Agenda
  , attendees       :: M.Map Int Attendee
  }

data Query a
  = ChangePage      Location   a
  | AdminMsg        CA.Message a
  | LoginMsg        CL.Message a
  | RegistrationMsg CR.Message a
  | WSMsg           String     a

tokenKey :: String
tokenKey = "token"

type Input = { token :: Maybe String, agenda :: Agenda, attendees :: M.Map Int Attendee }

component :: forall e. H.Component HH.HTML Query Input Void (Aff (LemmingPantsEffects e))
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i =
      { currentLocation: Home
      , token:           i.token
      , flash:           Nothing
      , agenda:          i.agenda
      , attendees:       i.attendees
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (LemmingPantsEffects e))
    render state =
      HH.div_
        (( case state.flash of
            Nothing -> []
            Just  s -> [ HH.div_ [ HH.text s ] ]
        ) <>
        [ HH.ul_
          [ HH.li_ [HH.a [HP.href "#registration"] [HH.text "Registration"]]
          , HH.li_ [HH.a [HP.href "#overhead"] [HH.text "Overhead"]]
          , HH.li_ [HH.a [HP.href "#admin"]    [HH.text "Admin"]]
          , HH.li_ [HH.a [HP.href "#login"]    [HH.text "Login"]]
          , HH.li_ [HH.a [HP.href "#/"]        [HH.text "Home"]]
          ]
        , locationToSlot state.currentLocation state
        ])
      where
        locationToSlot
          :: Location
          -> State
          -> H.ParentHTML Query ChildQuery ChildSlot (Aff (LemmingPantsEffects e))
        locationToSlot l s =
          case l of
            Registration -> go CP.cp1 1 CR.component s.token (HE.input RegistrationMsg)
            Overhead     -> go CP.cp2 2 CO.component ohs     (const Nothing)
            Admin        -> go CP.cp3 3 CA.component s'      (HE.input AdminMsg)
            Login        -> go CP.cp4 4 CL.component unit    (HE.input LoginMsg)
            Home         -> go CP.cp5 5 CH.component unit    (const Nothing)
          where
            go :: forall g i o
             . CP.ChildPath g ChildQuery Int ChildSlot
            -> Int
            -> H.Component HH.HTML g i o (Aff (LemmingPantsEffects e))
            -> i
            -> (o -> Maybe (Query Unit))
            -> H.ParentHTML Query ChildQuery ChildSlot (Aff (LemmingPantsEffects e))
            go cp p c i f = HH.slot' cp p c i f

            s'  = { token: s.token, agenda: s.agenda, attendees: s.attendees }
            ohs = { agenda: s.agenda, attendees: s.attendees }

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        ChangePage l next -> H.modify (_ {currentLocation = l}) *> pure next
        AdminMsg   m next ->
          case m of
            CA.Flash s -> flash s next
        LoginMsg   m next ->
          case m of
            CL.NewToken t ->
              H.modify (_ {token = Just t})
              *> H.liftEff (setItem localStorage tokenKey t)
              *> pure next
            CL.Flash s -> flash s next
        RegistrationMsg m next ->
          case m of
            CR.Flash s -> flash s next
        WSMsg s next ->
          H.liftAff (log s)
          *> H.get >>= (\state ->
          case runExcept (dispatcher state =<< readJSON' s) of
            Left  es -> flash (foldMap renderForeignError es) next
            Right s' -> H.put s' *> pure next
          )
      where
        flash s next =
          H.modify (_ {flash = Just s})
          *> pure next

        dispatcher
          :: State
          -> { channel :: String, event :: String, payload :: Foreign }
          -> F State
        dispatcher state r =
          case r.event of
            "agenda_item_insert"   -> handleAgendaItem   (readImpl r.payload) Insert state
            "agenda_item_update"   -> handleAgendaItem   (readImpl r.payload) Update state
            "attendee_insert"      -> handleAttendee     r.payload            Insert state
            "attendee_update"      -> handleAttendee     r.payload            Update state
            "speaker_insert"       -> handleSpeaker      (readImpl r.payload) Insert state
            "speaker_update"       -> handleSpeaker      (readImpl r.payload) Update state
            "speaker_queue_insert" -> handleSpeakerQueue (readImpl r.payload) Insert state
            "speaker_queue_update" -> handleSpeakerQueue (readImpl r.payload) Update state
            _                      -> fail (ForeignError "Anything can happen.")

        -- TODO: Make _ALL_ the go functions scream and shout if they can't update stuff.
        handleAgendaItem
          :: F { id      :: Int
               , title   :: String
               , content :: String
               , order_  :: Int
               , state   :: String
               }
             -> WSAction
             -> State
             -> F State
        handleAgendaItem fr action st =
          fr
            >>= \air -> go action air st.agenda
            <#> \r   -> st { agenda = AG.jumpToFirstActive r }
          where
            go Insert air ag = pure (AG.insert (newAI air) ag)
            go Update air ag =
              except (note (pure (ForeignError "ERROR: handleAgendaItem: Could not update AgendaItem!"))
                ((AG.modify air.id (\(AgendaItem x) ->
                  let (AgendaItem ai) = newAI air
                   in Just $ AgendaItem (ai { speakerQueues = x.speakerQueues })) ag)))

            newAI { id, title, content, order_, state } =
              AgendaItem {id, title, content, order_, state, speakerQueues: mempty }

        handleAttendee :: Foreign -> WSAction -> State -> F State
        handleAttendee fr action state =
            readImpl fr <#> \at -> state { attendees = go action at state.attendees }
          where
            go Insert a@(Attendee a') as = M.insert a'.id a as
            go Update a@(Attendee a') as = M.update (const (Just a)) a'.id as

        handleSpeaker
          :: F { id               :: Int
               , speaker_queue_id :: Int
               , attendee_id      :: Int
               , state            :: String
               , times_spoken     :: Maybe Int
               , agenda_item_id   :: Int
               }
             -> WSAction
             -> State
             -> F State
        handleSpeaker fr action st =
          fr >>= \r ->
            except (note
              (pure (ForeignError ("Modifying the speaker failed.")))
              (AG.modify
                r.agenda_item_id
                (AG.modifySQ r.speaker_queue_id (go action r))
                st.agenda)
              <#> \a' -> st { agenda = a' })
          where
            go Insert r sq = Just $ addSpeaker (newSpeaker r) sq
            go Update r sq = Just $ modifySpeaker r.id (const $ newSpeaker r) sq

            newSpeaker {id, attendee_id, state, times_spoken} =
              let ts = fromMaybe 0 times_spoken
               in Speaker { id, attendeeId: attendee_id, state, timesSpoken: ts }

        handleSpeakerQueue
          :: F { id             :: Int
               , agenda_item_id :: Int
               , state          :: String
               }
          -> WSAction
          -> State
          -> F State
        handleSpeakerQueue r action s =
          r >>= \{id, agenda_item_id, state} ->
          except (note
            (pure (ForeignError "Modifying the speaker queue failed."))
            (AG.modify agenda_item_id (go action {id, state}) s.agenda
              <#> \a' -> s { agenda = a' }))
          where
            go Insert {id, state} ai = Just (AG.pushSQ (SpeakerQueue {id, state, speaking: Nothing, speakers: []}) ai)
            go Update {id, state} ai =
              if state == "done"
                then AG.popSQIfMatchingId id ai
                else Nothing
