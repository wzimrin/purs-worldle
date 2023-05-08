module Shared.Utils where

import Prelude

import Data.Array as Array
import Data.Either (Either, either)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Random as Random

throwOnError :: forall m a b. MonadEffect m => (a -> String) -> Either a b -> m b
throwOnError toString = either (liftEffect <<< throw <<< toString) pure

foreign import debug :: forall a b. a -> b -> b

debug_ :: forall a. a -> a
debug_ a = debug a a

debugShow :: forall a b. Show a => a -> b -> b
debugShow a b = debug (show a) b

debugShow_ :: forall a. Show a => a -> a
debugShow_ a = debugShow a a

randomElement :: forall a. Array a -> Effect a
randomElement arr = do
  idx <- Random.randomInt 0 (Array.length arr - 1)
  maybe (throw "Array changed length") pure $ Array.index arr idx
