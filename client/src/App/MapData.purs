module App.MapData where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(TypeMismatch), (.:))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map as M
import Data.Tuple as T
import Data.String as S
import Data.Array as A
import Data.Maybe (fromMaybe)

newtype Point = Point { lat :: Number, long :: Number }
type PolygonCoords = Array (Array Point)
type MultiPolygonCoords = Array PolygonCoords
data Geometry = Polygon PolygonCoords | MultiPolygon MultiPolygonCoords
type Properties =
  { area :: Number
  , caplat :: Number
  , caplong :: Number
  , cntry_name :: String
  , capname :: String
  , gweyear :: Int
  }

type Feature = { geometry :: Geometry, properties :: Properties }
type FeatureCollection = { features :: Array Feature }

derive newtype instance showPoint :: Show Point
derive newtype instance eqPoint :: Eq Point
instance decodePoint :: DecodeJson Point where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [ long, lat ] -> pure $ Point { lat, long }
      _ -> Left $ TypeMismatch "Point"

derive instance eqGeometry :: Eq Geometry
derive instance genericGeometry :: Generic Geometry _
instance showGeometry :: Show Geometry where
  show = genericShow

instance decodeGeometry :: DecodeJson Geometry where
  decodeJson json = do
    obj <- decodeJson json
    geoType <- obj .: "type"
    case geoType of
      "Polygon" -> do
        coordinates <- obj .: "coordinates"
        map Polygon $ decodeJson coordinates
      "MultiPolygon" -> do
        coordinates <- obj .: "coordinates"
        map MultiPolygon $ decodeJson coordinates
      _ -> Left $ TypeMismatch "Geometry"

stopAt :: S.Pattern -> String -> String
stopAt p s = fromMaybe s $ A.head $ S.split p s

convertName :: String -> String
convertName = A.intercalate " " <<< A.reverse <<< S.split (S.Pattern ", ") <<< stopAt (S.Pattern "/") <<< stopAt (S.Pattern " (")

parse :: Json -> Either JsonDecodeError (M.Map String Feature)
parse json = do
  { features } <- decodeJson json :: Either JsonDecodeError FeatureCollection
  pure $ M.fromFoldable $ map toTuple $ A.filter validFeature features
  where
  toTuple feature = T.Tuple newName newFeature
    where
    newName = convertName feature.properties.cntry_name
    newFeature = feature { properties { cntry_name = newName } }
  validFeature feature = feature.properties.gweyear == 2019
