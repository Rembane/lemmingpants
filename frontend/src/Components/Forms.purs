module Components.Forms where

import Components.Forms.Field as F
import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Foreign.Object (Object, fromFoldable)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Prelude (type (~>), Unit, const, pure, unit, (*>), (<>), (>>=), (>>>))
import Web.Event.Event (Event, preventDefault)

type State = Unit

data Query a
  = SubmitForm Event a
  | ClearForm        a

data Message = FormSubmitted (Object String)

component
  :: forall m
   . MonadEffect m
  => String
  -> (Array (F.FieldComponent m))
  -> H.Component HH.HTML Query Unit Message m
component buttonLabel fields =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query F.Query Int m
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

  eval :: Query ~> H.HalogenM State Query F.Query Int Message m
  eval =
    case _ of
      SubmitForm e next ->
        -- See https://github.com/slamdata/purescript-halogen/issues/426
        -- for more inspiration.
        (H.liftEffect (preventDefault e)) *>
        (HQ.queryAll (F.GetNameAndValue Tuple)
          >>= M.values
          >>> fromFoldable
          >>> FormSubmitted
          >>> H.raise)
        *> pure next
      ClearForm next ->
        H.queryAll (H.action (F.UpdateField "")) *> pure next
