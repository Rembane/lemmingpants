module Components.Home where

import Halogen as H
import Halogen.HTML as HH

render :: forall p i. H.HTML p i
render =
  HH.p_ [HH.text "Home! :D"]

