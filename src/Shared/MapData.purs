module Shared.MapData where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(TypeMismatch), (.:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe as Maybe
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))

type Point = { lat :: Number, long :: Number }
type Geometry = Array (Array Point)
type Feature = { geometry :: Geometry, properties :: Properties }
type Features = Map Int Feature

newtype JPoint = JPoint Point
type JPolygonCoords = Array (Array JPoint)
type JMultiPolygonCoords = Array JPolygonCoords
data JGeometry = JPolygon JPolygonCoords | JMultiPolygon JMultiPolygonCoords
type Properties =
  { area :: Number
  , caplat :: Number
  , caplong :: Number
  , cntry_name :: String
  , capname :: String
  , gweyear :: Int
  , gwcode :: Int
  }

type JFeature = { geometry :: JGeometry, properties :: Properties }
type JFeatureCollection = { features :: Array JFeature }

derive newtype instance showPoint :: Show JPoint
derive newtype instance eqPoint :: Eq JPoint
instance decodePoint :: DecodeJson JPoint where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [ long, lat ] -> pure $ JPoint { lat, long }
      _ -> Left $ TypeMismatch "Point"

derive instance eqGeometry :: Eq JGeometry
derive instance genericGeometry :: Generic JGeometry _
instance showGeometry :: Show JGeometry where
  show = genericShow

instance decodeGeometry :: DecodeJson JGeometry where
  decodeJson json = do
    obj <- decodeJson json
    geoType <- obj .: "type"
    case geoType of
      "Polygon" -> do
        coordinates <- obj .: "coordinates"
        map JPolygon $ decodeJson coordinates
      "MultiPolygon" -> do
        coordinates <- obj .: "coordinates"
        map JMultiPolygon $ decodeJson coordinates
      _ -> Left $ TypeMismatch "Geometry"

stopAt :: String.Pattern -> String -> String
stopAt p s = Maybe.fromMaybe s $ Array.head $ String.split p s

convertName :: String -> String
convertName =
  stopAt (String.Pattern " (")
    >>> stopAt (String.Pattern "/")
    >>> String.split (String.Pattern ", ")
    >>> Array.reverse
    >>> Array.intercalate " "

convertPoint :: JPoint -> Point
convertPoint (JPoint point) = point

flattenGeometry :: JGeometry -> Geometry
flattenGeometry (JPolygon coords) = map (map convertPoint) coords
flattenGeometry (JMultiPolygon coords) = map (map convertPoint) $ join coords

convertFeature :: JFeature -> Feature
convertFeature { properties, geometry } = newFeature
  where
  newName = convertName properties.cntry_name
  newFeature =
    { properties: properties { cntry_name = newName }
    , geometry: flattenGeometry geometry
    }

parse :: Json -> Either JsonDecodeError Features
parse json = do
  { features } <- decodeJson json :: Either JsonDecodeError JFeatureCollection
  let validFeatures = Array.filter validFeature features
  let tuples = map (convertFeature >>> toTuple) validFeatures
  pure $ Map.fromFoldable $ tuples
  where
  toTuple feature = Tuple feature.properties.gwcode feature
  validFeature feature = feature.properties.gweyear == 2019
