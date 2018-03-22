module Utils where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit)

-- | Creates a form input with a label.
-- Type like this?
-- withLabel :: forall p f. (String -> String -> Unit -> f _ _) -> HP.InputType -> Boolean -> String -> String -> H.HTML p (f Unit)
withLabel q t r id lbl =
  HH.p_
    [ HH.label_
      [ HH.text lbl
      , HH.input
        [ HP.type_ t
        , HP.id_ id
        , HP.required r
        , HE.onValueInput (HE.input (q id))
        ]
      ]
    ]

