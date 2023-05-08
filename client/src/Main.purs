module Main where

import Prelude

import App.Game as Game
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Affjax.Web as AJ
import Affjax.ResponseFormat as AR
import Data.Either (either, Either)
import Effect.Class.Console (logShow)
import Effect.Class (class MonadEffect, liftEffect)
import App.MapData (parse)
import Effect.Exception (throw)
import Data.Map as M

ignore :: forall m a. Monad m => m a -> m Unit
ignore mv = mv >>= (\_ -> pure unit)

throwOnError :: forall m a b. MonadEffect m => (a -> String) -> Either a b -> m b
throwOnError toString = either (liftEffect <<< throw <<< toString) pure

main :: Effect Unit
main = HA.runHalogenAff do
  { body } <- AJ.get AR.json "/public/data.json" >>= throwOnError AJ.printError
  mapData <- throwOnError show $ parse body
  logShow mapData
  logShow $ map (\f -> f.properties) $ M.lookup "United States of America" mapData
  page <- HA.awaitBody
  ignore $ runUI Game.component unit page
