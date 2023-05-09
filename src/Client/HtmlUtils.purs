module Client.HtmlUtils where

import Prelude

import Halogen as H
import Halogen.HTML.Properties as HP

classes :: forall r i. Array String -> HP.IProp (class :: String | r) i
classes = map H.ClassName >>> HP.classes
