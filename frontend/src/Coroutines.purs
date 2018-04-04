module Coroutines
  ( consumerToQuery
  , routeProducer
  ) where

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Prelude (type (~>), Unit, pure, void, (*>), (<<<))
import Routing.Hash (matches)
import Routing.Match (Match)

-- | Turns Coroutine messages into Halogen queries.
consumerToQuery
  :: forall e f a
   . (f ~> Aff (HA.HalogenEffects e))
  -> (a -> Unit -> f Unit)
  -> CR.Consumer a (Aff (HA.HalogenEffects e)) Unit
consumerToQuery query fun = CR.consumer \msg ->
  query (H.action (fun msg))
  *> pure Nothing

-- | Catches the production of routes from purescript-routes
-- | and turns them into a coroutine producer.
routeProducer
  :: forall e a
   . Match a
  -> CR.Producer a (Aff (avar :: AVAR, dom :: DOM, ref :: REF | e)) Unit
routeProducer ls = CRA.produce \emit ->
  void (matches ls (\_ -> emit <<< Left))
