module Server.Main where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Payload.ResponseTypes as Response
import Payload.Server as Payload
import Payload.Server.Handlers as Handlers
import Payload.Spec (GET, Spec(Spec))
import Shared.API (APIRoutes)
import Shared.MapData (Geometry, Properties, Features, parse)
import Shared.Utils (debug, throwOnError)

spec
  :: Spec
       { js ::
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
       | APIRoutes
       }
spec = Spec

countries :: Features -> {} -> Aff (Array Properties)
countries mapData {} = pure $ Array.fromFoldable $ map _.properties $ Map.values mapData

countryMap :: Features -> { params :: { gwcode :: Int } } -> Aff (Either String Geometry)
countryMap mapData { params: { gwcode } } = pure $ case Map.lookup gwcode mapData of
  Just feature -> debug (Array.length feature.geometry) $ Right feature.geometry
  Nothing -> Left "Country not found"

public :: { params :: { path :: List String } } -> Aff (Either Response.Failure Handlers.File)
public { params: { path } } = Handlers.directory "public" path

js :: { params :: { path :: List String } } -> Aff (Either Response.Failure Handlers.File)
js { params: { path } } = Handlers.directory "dist" path

index :: {} -> Aff Handlers.File
index _ = Handlers.file "public/index.html" {}

getFile :: String -> Effect String
getFile = readTextFile UTF8

main :: Effect Unit
main = do
  json <- jsonParser <$> getFile "data/data.json" >>= throwOnError identity
  mapData <- throwOnError show $ parse json
  Payload.launch spec $ handlers mapData
  where
  handlers mapData = { countries: countries mapData, countryMap: countryMap mapData, public, js, index }
