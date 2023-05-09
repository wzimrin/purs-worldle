module Client.MapMath where

import Prelude

import Data.Foldable (foldr)
import Data.Int as Int
import Data.Number (asin, atan, cos, floor, infinity, log, pi, pow, sin, sqrt, tan)
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

haversineDistance :: Map.Point -> Map.Point -> Int
haversineDistance { lat: latA, long: longA } { lat: latB, long: longB } =
  Int.floor $ 2.0 * r * asin (sqrt $ a + b)
  where
  latAR = toRadians latA
  longAR = toRadians longA
  latBR = toRadians latB
  longBR = toRadians longB
  r = 6371.0
  a = pow (sin $ (latBR - latAR) / 2.0) 2.0
  b = cos latAR * cos latBR * pow (sin $ (longBR - longAR)) 2.0

-- Direction from p1 to p2
angularDirection :: Map.Point -> Map.Point -> String
angularDirection p1 p2 | p1 == p2 = "X"
angularDirection { lat: latA, long: longA } { lat: latB, long: longB }
  | longA == longB = if latA < latB then "N" else "S"
  | otherwise = prefix <> suffix
      where
      diffLat = latB - latA
      diffLong = longB - longA
      ratio = diffLat / diffLong
      angle = atan ratio
      prefix =
        if angle > pi / 8.0 || angle < pi / (-8.0) then
          if latA < latB then "N" else "S"
        else ""
      suffix =
        if angle < pi * 3.0 / 8.0 && angle > pi * (-3.0) / 8.0 then
          if longA < longB then "E" else "W"
        else ""
