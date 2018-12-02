module Lemmingpants where

import Prelude

import Components.Admin as CA
import Components.Admin.ListAttendees as CAL
import Components.Home as CH
import Components.Login as CL
import Components.Overhead as CO
import Components.Registration as CR
import Control.Alt ((<|>))
import Control.Monad.Except (except, runExcept)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Either.Nested (Either6)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Lens (over, set, traverseOf)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.String (toLower)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (F, Foreign, ForeignError(..), fail, readString, renderForeignError, unsafeFromForeign)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Match (Match, lit)
import Simple.JSON (readImpl, readJSON')
import Type.Prelude (SProxy(..))
import Types.Agenda as AG
import Types.Attendee as AT
import Types.Flash as FL
import Types.Lens (_withId)
import Types.Speaker as S
import Types.SpeakerQueue as SQ
import Types.Token (Payload(..), Token(..))
import Types.Token as TT
import Web.HTML (Window)

type ChildQuery = Coproduct6 CR.Query CO.Query CA.Query CAL.Query CL.Query CH.Query
type ChildSlot  = Either6 Int Int Int Int Int Int

data WSAction
  = Insert
  | Update

data Location
  = Home
  | Registration
  | Overhead
  | Admin
  | AdminListAttendees
  | Login

instance shLoc :: Show Location where
  show
    = case _ of
        Home               -> "Home"
        Registration       -> "Registration"
        Overhead           -> "Overhead"
        Admin              -> "Admin"
        AdminListAttendees -> "Admin: List attendees"
        Login              -> "Login"

locations :: Match Location
locations
  =   (Registration       <$ lit "registration")
  <|> (Overhead           <$ lit "overhead")
  <|> (Admin              <$ lit "admin")
  <|> (Admin              <$ lit "admin")
  <|> (AdminListAttendees <$ lit "adminAttendees")
  <|> (Login              <$ lit "login")
  <|> (Home               <$ lit "")

type State =
  { currentLocation :: Location
  , token           :: TT.Token
  , flash           :: Maybe FL.Flash
  , agenda          :: AG.Agenda
  , attendees       :: AT.AttendeeDB
  , showRegForm     :: Boolean
  , window          :: Window
  }

data Query a
  = ChangePage      Location   a
  | SignOut                    a
  | AdminMsg        CA.Message a
  | LoginMsg        CL.Message a
  | RegistrationMsg CR.Message a
  | WSMsg           Foreign    a

type Input = { token :: TT.Token, agenda :: AG.Agenda, attendees :: AT.AttendeeDB, window :: Window }

component :: H.Component HH.HTML Query Input Void Aff
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
      , showRegForm:     true
      , window:          i.window
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render state =
      HH.div
        [ HP.id_ (toLower (show state.currentLocation)) ]
        [ HH.nav_
          [ HH.menu_ (
            [ HH.li_ [HH.a [HP.href "#"]             [HH.text "Home"]]
            , HH.li_ [HH.a [HP.href "#registration"] [HH.text "Registration"]]
            , HH.li_ [HH.a [HP.href "#overhead"]     [HH.text "Overhead"]]
            ] <> (if pl.role == "admin_user"
                   then [ HH.li_
                      [ HH.a [HP.href "#admin"] [HH.text "Admin"]
                      , HH.menu
                        [ HP.id_ "submenu" ]
                        [ HH.li_ [ HH.a [HP.href "#adminAttendees"] [HH.text "List attendees"] ] ]
                      ]
                    ]
                    else mempty)
            <> [ loginlogoutlink state ])
          ]
        , HH.div
          [HP.class_ (HH.ClassName "container")]
          (maybe [] (\(FL.Flash f) -> [ HH.div [ HP.id_ "flash", (HP.class_ (HH.ClassName (show f.typ))) ] [ HH.text f.msg ] ]) state.flash
            <> [locationToSlot state.currentLocation state])
        ]
      where
        (Payload pl) = let (Token t) = state.token in t.payload

        loginlogoutlink
          :: State
          -> H.ParentHTML Query ChildQuery ChildSlot Aff
        loginlogoutlink s =
          let (TT.Token {payload}) = s.token
              (TT.Payload p)       = payload
           in if p.role == "admin_user"
                -- Blergh... hack... blergh...
                -- TODO: Unhack this!
                then HH.li_ [HH.a [HP.href "#signout", HE.onClick (HE.input_ SignOut)] [HH.text "Sign out"]]
                else HH.li_ [HH.a [HP.href "#login"]   [HH.text "Login"]]

        locationToSlot
          :: Location
          -> State
          -> H.ParentHTML Query ChildQuery ChildSlot Aff
        locationToSlot l s =
          case l of
            Registration       -> go CP.cp1 1 CR.component  rs   (HE.input RegistrationMsg)
            Overhead           -> go CP.cp2 2 CO.component  ohs  (const Nothing)
            Admin              -> go CP.cp3 3 CA.component  s'   (HE.input AdminMsg)
            AdminListAttendees -> go CP.cp4 4 CAL.component alas (const Nothing)
            Login              -> go CP.cp5 5 CL.component  ls   (HE.input LoginMsg)
            Home               -> go CP.cp6 6 CH.component  unit (const Nothing)
          where
            go :: forall g i o
               . CP.ChildPath g ChildQuery Int ChildSlot
              -> Int
              -> H.Component HH.HTML g i o Aff
              -> i
              -> (o -> Maybe (Query Unit))
              -> H.ParentHTML Query ChildQuery ChildSlot Aff
            go cp p c i f = HH.slot' cp p c i f

            ls   = { token: s.token }
            rs   = { token: s.token, showForm: s.showRegForm }
            s'   = { token: s.token, agenda: s.agenda, attendees: s.attendees }
            ohs  = { agenda: s.agenda, attendees: s.attendees }
            alas = { attendees: s.attendees }

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
    eval =
      case _ of
        ChangePage l next ->
          H.modify (_ {currentLocation = l, flash = Nothing, showRegForm = true}) *> pure next
        SignOut      next -> do
          s <- H.get
          H.liftAff (TT.removeToken s.window)
          pure next
        AdminMsg   m next ->
          case m of
            CA.Flash s -> flash s next
        LoginMsg   m next ->
          case m of
            CL.NewToken t -> do
              s <- H.modify (_ {token = t})
              H.liftAff (TT.saveToken s.window t)
              pure next
            CL.Flash s -> flash s next
        RegistrationMsg m next ->
          case m of
            CR.FrmVsbl b -> H.modify (\s -> s { showRegForm = b }) *> pure next
            CR.Flash   s -> flash s next
        WSMsg fr next -> do
          -- Log the message to the console.
          liftEffect $ log $ unsafeFromForeign fr
          state <- H.get
          -- In case you think this is inefficient and want to change
          -- `readJSON' =<< readString` into `read'`, do know that
          -- there is a bug that makes the parsing fail if you only
          -- run read' on your foreign data. I don't know why, but I
          -- intend to find out.
          --
          -- Regards, A.
          case runExcept (dispatcher state =<< readJSON' =<< readString fr) of
            Left  es -> flash (FL.mkFlash (foldMap renderForeignError es) FL.Error) next
            Right s' -> H.put s' *> pure next
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
            "attendee_insert"      -> handleAttendee     (readImpl r.payload)        state
            "attendee_update"      -> handleAttendee     (readImpl r.payload)        state
            "speaker_insert"       -> handleSpeaker      (readImpl r.payload) Insert state
            "speaker_update"       -> handleSpeaker      (readImpl r.payload) Update state
            "speaker_queue_insert" -> handleSpeakerQueue (readImpl r.payload) Insert state
            "speaker_queue_update" -> handleSpeakerQueue (readImpl r.payload) Update state
            _                      -> fail (ForeignError "Anything can happen.")

        -- TODO: Make _ALL_ the go functions scream and shout if they can't update stuff.
        handleAgendaItem
          :: F { id         :: Int
               , supertitle :: Maybe String
               , title      :: String
               , order_     :: Int
               , state      :: String
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
                (traverseOf (AG._AgendaItems <<< _withId air.id) (\(AG.AgendaItem {speakerQueues}) ->
                  Just $ set (_Newtype <<< prop (SProxy :: SProxy "speakerQueues")) speakerQueues (newAI air))
                  ag))

            newAI { id, supertitle, title, order_, state } =
              AG.AgendaItem { id, supertitle, title, order_, state, speakerQueues: mempty }

        handleAttendee :: F AT.Attendee -> State -> F State
        handleAttendee fr state =
            fr <#> \at -> state { attendees = AT.insertAttendee at state.attendees }

        handleSpeaker
          :: F { id               :: Int
               , speaker_queue_id :: Int
               , attendee_id      :: Int
               , state            :: S.SpeakerState
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
              (traverseOf
                (prop (SProxy :: SProxy "agenda") <<< AG._AgendaItems <<< _withId r.agenda_item_id <<< AG._SpeakerQueues <<< _withId r.speaker_queue_id)
                (Just <<< go action r)
                st))
          where
            go Insert r = over SQ._Speakers $ A.insert $ newSpeaker r
            go Update r = set (SQ._Speakers <<< _withId r.id) (newSpeaker r)

            newSpeaker {id, attendee_id, state, times_spoken} =
              S.Speaker { id, attendeeId: attendee_id, state, timesSpoken: fromMaybe 0 times_spoken }

        handleSpeakerQueue
          :: F { id             :: Int
               , agenda_item_id :: Int
               , state          :: SQ.SpeakerQueueState
               }
          -> WSAction
          -> State
          -> F State
        handleSpeakerQueue r action s =
          r >>= \{id, agenda_item_id, state} ->
          except (note
            (pure (ForeignError "Modifying the speaker queue failed."))
            (traverseOf (prop (SProxy :: SProxy "agenda") <<< AG._AgendaItems <<< _withId agenda_item_id) (go action {id, state}) s))
          where
            go Insert {id, state} ai = Just $ AG.pushSQ (SQ.SpeakerQueue {id, state, speakers: mempty}) ai
            go Update {id, state} ai =
              case state of
                SQ.Done -> AG.popSQIfMatchingId id ai
                _       -> Nothing
