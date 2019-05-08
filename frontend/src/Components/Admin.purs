module Components.Admin where

import Prelude

import Components.Admin.ListAttendees as CAL
import Components.Admin.ManageAgenda as CAA
import Components.Admin.MeetingManager as CAM
import Control.Alt ((<|>))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Match (Match, end, lit)
import Types.Agenda (Agenda)
import Types.Attendee (AttendeeDB)
import Types.Flash as FL
import Types.Token (Payload(..), Token(..))

type ChildQuery = Coproduct2 CAM.Query CAA.Query
type ChildSlot = Either2 Unit Unit

data Location
  = MeetingManager
  | ListAttendees
  | ManageAgenda

instance shLoc :: Show Location where
  show
    = case _ of
        MeetingManager -> "Admin"
        ListAttendees  -> "Admin: List attendees"
        ManageAgenda   -> "Agenda"

locations :: Match Location
locations
  =   (MeetingManager <$ end)
  <|> (ListAttendees  <$ lit "attendees" <* end)
  <|> (ManageAgenda   <$ lit "agenda" <* end)

type State =
  { agenda    :: Agenda
  , token     :: Token
  , attendees :: AttendeeDB
  , location  :: Location
  }

type Input = State

data Query a
  = GotNewState       State       a
  | MeetingManagerMsg CAM.Message a
  | ManageAgendaMsg   CAA.Message a

data Message
  = Flash (Maybe FL.Flash)

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: HE.input GotNewState
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render {agenda, token, attendees, location } =
      if role == "admin_user"
        then
          case location of
            MeetingManager ->
              HH.slot'
                CP.cp1
                unit
                CAM.component
                {agenda, token, attendees}
                (HE.input MeetingManagerMsg)
            ListAttendees ->
              CAL.render {attendees}
            ManageAgenda ->
              HH.slot' CP.cp2 unit CAA.component {agenda, token} (HE.input ManageAgendaMsg)
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
        (Payload {role}) = let (Token {payload}) = token in payload

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
    eval =
      case _ of
        MeetingManagerMsg (CAM.Flash m) next ->
          H.raise (Flash m) *> pure next
        ManageAgendaMsg (CAA.Flash m) next ->
          H.raise (Flash m) *> pure next
        GotNewState s next ->
          H.put s *> pure next
