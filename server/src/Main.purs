module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Payload.Server as Payload
import Payload.Server.Handlers as Handlers
import Payload.Spec (Spec(Spec), GET)
import Payload.ResponseTypes as Response
import Data.List (List)
import Data.Either (Either)

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
       , js ::
           GET "/js/<..path>"
             { params :: { path :: List String }
             , response :: Handlers.File
             }
       , public ::
           GET "/public/<..path>"
             { params :: { path :: List String }
             , response :: Handlers.File
             }
       , index ::
           GET "/"
             { params :: {}, response :: Handlers.File }
       }
spec = Spec

getMessages :: { params :: { id :: Int }, query :: { limit :: Int } } -> Aff (Array Message)
getMessages { params: { id }, query: { limit } } = pure
  [ { id: 1, text: "Hey " <> show id }, { id: 2, text: "Limit " <> show limit } ]

public :: { params :: { path :: List String } } -> Aff (Either Response.Failure Handlers.File)
public { params: { path } } = Handlers.directory "../client/public" path

js :: { params :: { path :: List String } } -> Aff (Either Response.Failure Handlers.File)
js { params: { path } } = Handlers.directory "../client/dist" path

index :: {} -> Aff Handlers.File
index _ = Handlers.file "../client/public/index.html" {}

main :: Effect Unit
main = Payload.launch spec handlers
  where
  handlers = { getMessages, public, js, index }
