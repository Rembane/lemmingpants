module Views.Admin where

import Types

import Control.Monad.Aff (Aff)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Effects (LemmingPantsEffects)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (type (~>), Unit, bind, const, pure, show, (*>), (+), (-), (<$>), (<>), (>), (>>=))
import Simple.JSON (read, readJSON, writeJSON)

type State =
  { agendaItems   :: Array (AgendaItem)
  , currAgendaIdx :: Int -- ^ The index of the current agenda item. It is 0 if none is active. Which is a potential bug.
  , token         :: Maybe String
  }

data Query a
  = Initialize a
  | Finalize   a
  | Previous   a
  | Next       a

data Message
  = Flash String

component :: forall e. H.Component HH.HTML Query (Maybe String) Message (Aff (LemmingPantsEffects e))
component =
  H.lifecycleComponent
    { initialState: \i -> {agendaItems: mempty, currAgendaIdx: 0, token: i }
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Supreme council interface"]
        , HH.h2_
          [ HH.button
            [ HE.onClick (HE.input_ Previous) ]
            [ HH.text "⇐" ]
          , HH.text " "
          , HH.text (fromMaybe "OUT OF INDEX EXCEPTION!!!" ((\(AgendaItem a) -> a.title) <$> A.index state.agendaItems state.currAgendaIdx))
          , HH.text " "
          , HH.button
            [ HE.onClick (HE.input_ Next) ]
            [ HH.text "⇒" ]
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Message (Aff (LemmingPantsEffects e))
    eval =
      case _ of
        Initialize next ->
           (H.liftAff ((\r -> parseAgendaItem r.response) <$> AX.get "http://localhost:3000/agenda_item?order=order_.asc")
           >>= \as ->
               case as of
                 Left  es  -> H.raise (Flash (foldMap renderForeignError es))
                 Right as' -> let i = fromMaybe 0 (A.findIndex (\(AgendaItem a) -> a.active) as')
                               in H.modify (_ { agendaItems = as', currAgendaIdx = i })
           )
           *> pure next
        Finalize next ->
          pure next
        Previous next ->
          setCurrentAgendaItem (_-1)
          *> pure next
        Next next ->
          setCurrentAgendaItem (_+1)
          *> pure next

      where
        parseAgendaItem :: Foreign -> Either MultipleErrors (Array AgendaItem)
        parseAgendaItem = read

        setCurrentAgendaItem :: (Int -> Int) -> H.ComponentDSL State Query Message (Aff (LemmingPantsEffects e)) Unit
        setCurrentAgendaItem f = do
          s <- H.get
          case s.token of
            Nothing -> H.raise (Flash "You are not logged in. Please login.")
            Just t  ->
              case A.index s.agendaItems (f s.currAgendaIdx) of
                Nothing             -> H.raise (Flash ("NO AGENDA ITEMS AT INDEX: " <> show (f s.currAgendaIdx)))
                Just (AgendaItem a) -> do
                  let req = AX.defaultRequest
                  r <- H.liftAff (AX.affjax (
                         req { url     = "http://localhost:3000/rpc/set_current_agenda_item"
                             , headers =
                                 req.headers <>
                                   [ ContentType (MediaType "application/json")
                                   , RequestHeader "Authorization" ("Bearer " <> t)
                                   ]
                             , method  = Left POST
                             , content = Just (writeJSON ({ "id" : a.id }))
                             }))
                  case readJSON r.response of
                    Left es -> H.raise (Flash (foldMap renderForeignError es))
                    Right i ->
                      if i > 0
                        then H.modify (_ {currAgendaIdx = f s.currAgendaIdx })
                        else H.raise (Flash "OUT OF INDEX ERROR! Or something equally terrible.")

