module Client.MapMath where

import Prelude

import Data.Foldable (foldr)
import Data.Number (cos, infinity, log, pi, tan, floor)
import Shared.MapData as Map

type BBox = { xMin :: Number, xMax :: Number, yMin :: Number, yMax :: Number }

type Point = { x :: Number, y :: Number }
type Shape = Array Point

toRadians :: Number -> Number
toRadians n = n / 180.0 * pi

toDegrees :: Number -> Number
toDegrees n = n / pi * 180.0

mercator :: Map.Point -> Point
mercator { lat, long } = { x: long, y: toDegrees $ log $ tan latR + 1.0 / cos latR }
  where
  latR = toRadians lat

bound :: Number -> Number -> Number
bound a b = a - m * b
  where
  m = floor (a / b)

projectPoint :: Map.Properties -> Map.Point -> Point
projectPoint { caplong } { lat, long } = mercator { lat, long: rotatedLong }
  where
  rotatedLong = bound (long + 540.0 - caplong) 360.0

projectShapes :: Map.Properties -> Map.Geometry -> Array Shape
projectShapes properties geometry = map (map $ projectPoint properties) geometry

extremes :: Array Number -> { minimum :: Number, maximum :: Number }
extremes = foldr op { minimum: infinity, maximum: (-infinity) }
  where
  op n { minimum, maximum } = { minimum: min n minimum, maximum: max n maximum }

bboxOf :: Array Shape -> BBox
bboxOf arr = { xMin, xMax, yMin, yMax }
  where
  points = join arr
  { minimum: xMin, maximum: xMax } = extremes $ map _.x points
  { minimum: yMin, maximum: yMax } = extremes $ map _.y points

valInBounds :: Number -> Number -> Number -> Number -> Number
valInBounds val min max len = ((val - min) / (max - min)) * len

pointInBBox :: BBox -> Number -> Number -> Point -> Point
pointInBBox { xMin, xMax, yMin, yMax } width height { x, y } =
  { x: valInBounds x xMin xMax usableWidth + (width - usableWidth) / 2.0
  , y: height - valInBounds y yMin yMax usableHeight - (height - usableHeight) / 2.0
  }
  where
  bboxAspectRatio = (xMax - xMin) / (yMax - yMin)
  canvasAspectRatio = width / height
  usableWidth = if bboxAspectRatio > canvasAspectRatio then width else height * bboxAspectRatio
  usableHeight = if bboxAspectRatio > canvasAspectRatio then width / bboxAspectRatio else height

mapOf :: Map.Properties -> Map.Geometry -> Number -> Number -> Array Shape
mapOf properties geometry width height = map (map $ pointInBBox bbox width height) unbounded
  where
  unbounded = projectShapes properties geometry
  bbox = bboxOf unbounded
