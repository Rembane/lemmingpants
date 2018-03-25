module Forms where

import Control.Monad.Aff (Aff)
import DOM.Event.Event (Event, preventDefault)
import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Effects (LemmingPantsEffects)
import Forms.Field as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Prelude (type (~>), Unit, const, pure, unit, (*>), (<>), (>>=), (>>>))

type State = Unit

data Query a
  = SubmitForm Event a

data Message = FormSubmitted (SM.StrMap String)

component
  :: forall e
   . String
  -> (Array (F.FieldComponent (Aff (LemmingPantsEffects e))))
  -> H.Component HH.HTML Query Unit Message (Aff (LemmingPantsEffects e))
component buttonLabel fields =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query F.Query Int (Aff (LemmingPantsEffects e))
  render _ =
    HH.form
      [ HE.onSubmit (HE.input SubmitForm) ]
      (( A.mapWithIndex (\i c -> HH.slot i c Nothing (const Nothing)) fields )
      <>
      [ HH.p_
        [ HH.input
          [ HP.type_ HP.InputSubmit
          , HP.value buttonLabel
          ]
        ]
      ])

  eval :: Query ~> H.HalogenM State Query F.Query Int Message (Aff (LemmingPantsEffects e))
  eval =
    case _ of
      SubmitForm e next ->
        -- See https://github.com/slamdata/purescript-halogen/issues/426
        -- for more inspiration.
        (H.liftEff (preventDefault e)) *>
        (HQ.queryAll (F.GetNameAndValue Tuple)
          >>= M.values
          >>> SM.fromFoldable
          >>> FormSubmitted
          >>> H.raise)
        *> pure next
