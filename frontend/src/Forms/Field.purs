module Forms.Field
  ( mkField
  , FieldComponent
  , Query(..)
  , State
  , Message
  ) where

import DOM.HTML.Indexed (HTMLinput)
import Data.Const (Const)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, const, id, pure, (*>), (<$>), (<<<), (<>))

data Query a
  = UpdateField String a
  | GetNameAndValue (String -> String -> a)

data Message = FieldUpdated String String

type State = Maybe String

type FieldComponent m = H.Component HH.HTML Query State Message m

mkField
  :: forall m
   . String
  -> String
  -> Array (HP.IProp HTMLinput (Query Unit))
  -> FieldComponent m
mkField name label props =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render value =
      HH.p_
        [ HH.label_
          [ HH.text label
          , HH.input
            ([ HP.value (fromMaybe "" value)
            , HP.name name
            , HE.onValueInput (HE.input UpdateField)
            ] <> props)
          ]
        ]

    eval :: Query ~> H.HalogenM State Query (Const Void) Void Message m
    eval =
      case _ of
        UpdateField v next ->
          H.modify (const (Just v))
          *> H.raise (FieldUpdated name v)
          *> pure next
        GetNameAndValue reply ->
          (reply name <<< fromMaybe "") <$> H.get
