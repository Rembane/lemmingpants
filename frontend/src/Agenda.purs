module Agenda where

import Prelude (type (~>), Unit, Void, bind, const, id, pure, show, unit, ($), (*>), (+))
import Types (SpeakerQueue)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX
import Simple.JSON (class ReadForeign, readJSON)

newtype AgendaItem = AgendaItem
  { order             :: Int
  , title             :: String
  , content           :: String
  , speakerQueueStack :: Array SpeakerQueue
  }

derive instance ntAI :: Newtype AgendaItem _
derive newtype instance rfAI :: ReadForeign AgendaItem

type State = { current :: AgendaItem }
type Input = State

data Query a
  = Previous a
  | Next a

type AgendaEffects e = (Aff (ajax :: AX.AJAX, console :: CONSOLE | e))

component :: forall e. H.Component HH.HTML Query Input Void (AgendaEffects e)
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
      [ HH.h2_
        [ HH.button
          [ HE.onClick (HE.input_ Previous) ]
          [ HH.text "|<" ]
        , HH.text "&sect;"
        , HH.text (show (currentItem.order + 1))
        , HH.text currentItem.title
        , HH.button
            [ HE.onClick (HE.input_ Next) ]
            [ HH.text ">|" ]
        ]
      ]
    where
      currentItem = let (AgendaItem a) = state.current
                     in a

  eval :: Query ~> H.ComponentDSL State Query Void (AgendaEffects e)
  eval =
    case _ of
      Previous next -> rpcHelper "/agenda/previous/" *> pure next
      Next     next -> rpcHelper "/agenda/next/"     *> pure next

    where
      rpcHelper :: String -> H.ComponentDSL State Query Void (AgendaEffects e) Unit
      rpcHelper url = do
        r <- H.liftAff $ AX.post url unit
        case readJSON r.response of
          Left es -> H.liftAff $ logShow es
          Right v -> H.modify (_ { current = v } )
