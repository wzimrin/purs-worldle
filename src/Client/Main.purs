module Client.Main where

import Prelude

import Client.Game as Game
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

ignore :: forall m a. Monad m => m a -> m Unit
ignore mv = mv >>= (\_ -> pure unit)

main :: Effect Unit
main = HA.runHalogenAff do
  page <- HA.awaitBody
  ignore $ runUI Game.component unit page
