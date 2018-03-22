module Views.Terminal where

import Control.Monad.Aff (Aff)
import Data.Const (Const)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(Nothing))
import Data.StrMap (StrMap, empty, insert)
import Debug.Trace (traceA)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, const, pure, (*>), (<$>), (<>), (=<<))
import Utils (withLabel)

type State =
  { formState :: StrMap String }

data Query a
  = UpdateField String String a
  | SubmitForm a

component :: forall e. H.Component HH.HTML Query Unit Void (Aff (HA.HalogenEffects e))
component =
  H.component
    { initialState: const { formState: empty }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [ HH.h1_ [HH.text "Terminal"]
        , HH.form
          [ HE.onSubmit (HE.input_ SubmitForm) ]
          [ withLabel UpdateField HP.InputNumber true  "number" "Number"
          , withLabel UpdateField HP.InputText   true  "cid"    "CID"
          , withLabel UpdateField HP.InputText   true  "name"   "Full name"
          , withLabel UpdateField HP.InputText   false "nick"   "Nickname, if any"
          , HH.p_
            [ HH.input
              [ HP.type_ HP.InputSubmit
              , HP.value "I am attending this meeting!"
              ]
            ]
          ]
        ]

    eval :: Query ~> H.HalogenM State Query (Const Void) Void Void (Aff (HA.HalogenEffects e))
    eval =
      case _ of
        UpdateField k v next ->
          H.modify (\s -> s { formState = insert k v s.formState }) *>
          pure next
        SubmitForm next      ->
          (traceA =<< foldMapWithIndex (\k v -> k <> ": " <> v <> "\n") <$> H.gets (\s -> s.formState))
          *> pure next
