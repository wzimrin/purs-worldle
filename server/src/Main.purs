module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Payload.Server as Payload
import Payload.Server.Handlers as Handlers
import Payload.Spec (Spec(Spec), GET)

type Message =
  { id :: Int
  , text :: String
  }

spec
  :: Spec
       { getMessages ::
           GET "/users/<id>/messages?limit=<limit>"
             { params :: { id :: Int }
             , query :: { limit :: Int }
             , response :: Array Message
             }
       , index ::
           GET "/"
             { response :: String
             }
       }
spec = Spec

getMessages :: { params :: { id :: Int }, query :: { limit :: Int } } -> Aff (Array Message)
getMessages { params: { id }, query: { limit } } = pure
  [ { id: 1, text: "Hey " <> show id }, { id: 2, text: "Limit " <> show limit } ]

index :: {} -> Aff String
index {} = pure "Hello World!"

main :: Effect Unit
main = Payload.launch spec handlers
  where
  handlers = { getMessages, index }
