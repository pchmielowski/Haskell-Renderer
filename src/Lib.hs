module Lib
  ( someFunc
  , Vector
  , Triangle
  ) where

import Common
import Data.List
import Data.Maybe
import Data.Ord
import Data.Vec3
import Reader

cameraSource :: Vector
cameraSource = (0, 5, 0)

cameraTarget :: Int -> Int -> Vector
cameraTarget x y =
  normalize
    ( (2 * ((x' + 0.5) / width') - 1) * angle * width' / height'
    , -1
    , (1 - 2 * (y' + 0.5) / height') * angle)
  where
    angle = tan $ pi * 0.5 * fieldOfView / 180
    fieldOfView = 60
    x' = fromIntegral x
    y' = fromIntegral y
    width' = fromIntegral width
    height' = fromIntegral height

light :: Vector
light = (20, 10, -1)

data Ray = Ray
  { orig :: Vector
  , direction :: Vector
  }

data Intersection = Intersection
  { point :: Vector
  , normal :: Vector
  }

-- source: https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
intersection :: Ray -> Triangle -> Maybe Intersection
intersection ray triangle =
  if a > -eps && a < eps
    then Nothing
    else if u < 0 || u > 1
           then Nothing
           else if v < 0 || u + v > 1
                  then Nothing
                  else if t > eps
                         then Just $ Intersection pnt nrml
                         else Nothing
  where
    eps = 0.0000001
    edge1 = triangle !! 1 <-> (triangle !! 0)
    edge2 = triangle !! 2 <-> (triangle !! 0)
    h = direction ray >< edge2
    a = edge1 .* h
    f = 1 / a
    s = orig ray <-> (triangle !! 0)
    u = f * (s .* h)
    q = s >< edge1
    v = f * ((direction ray) .* q)
    t = f * (edge2 .* q)
    pnt = ((orig ray) <+> (direction ray) .^ t)
    nrml = normalize $ edge1 >< edge2

width :: Int
width = 800

height :: Int
height = 600

header :: String
header = "P3\n" ++ show (width) ++ " " ++ show (height) ++ "\n255\n"

cameraRay :: Int -> Int -> Ray
cameraRay x y = Ray cameraSource $ cameraTarget x y

closest :: [Intersection] -> Maybe Intersection
closest [] = Nothing
closest intersections =
  Just $ minimumBy (comparing distanceFromOrigin) intersections
  where
    distanceFromOrigin = norm . (cameraSource <->) . point

intersections :: Ray -> [Triangle] -> [Intersection]
intersections = (catMaybes .) . map . intersection

triangles = concat [cone, ground, wall]
  where
    cone =
      [ [(0, 0, z1), (-1, -1, z0), (0, -r, z0)]
      , [(0, 0, z1), (0, -r, z0), (1, -1, z0)]
      , [(0, 0, z1), (1, -1, z0), (r, 0, z0)]
      , [(0, 0, z1), (r, 0, z0), (1, 1, z0)]
      , [(0, 0, z1), (1, 1, z0), (0, r, z0)]
      , [(0, 0, z1), (0, r, z0), (-1, 1, z0)]
      , [(0, 0, z1), (-1, 1, z0), (-r, 0, z0)]
      , [(0, 0, z1), (-r, 0, z0), (-1, -1, z0)]
      ]
    ground =
      parseTriangles
        "v -2 -2 -2\n\
        \v 2 2 -2\n\
        \v -2 2 -2\n\
        \v 2 -2 -2\n\
        \f 1 2 3\n\
        \f 1 4 2"
    wall =
      [ [(2, -2, 2), (2, 2, z0), (2, -2, z0)]
      , [(2, 2, 2), (2, -2, 2), (2, 2, z0)]
      ]
    r = sqrt 2
    z1 = 0
    z0 = -2

pixel :: Int -> Int -> [Int]
pixel x y =
  take 3 $ repeat $ color $ closest $ intersections (cameraRay x y) triangles
  where
    color intersection =
      case intersection of
        Just it ->
          let intensity =
                if isInShadow $ point it
                  then 0
                  else (normal it) .* (lightDirection (point it))
          in max 0 $ round $ intensity * 256
        Nothing -> 0
    direction = (normalize .) . (<->)
    lightDirection = direction light
    lightRay point = Ray light $ lightDirection point -- .^ (-1)
    isInShadow point = False --not $ null $ intersections (lightRay point) triangles

row :: Int -> [Int]
row y = concat $ map (\x -> pixel x y) [1 .. width]

image :: [Int]
image = concat $ map row [1 .. height]

body :: String
body = intercalate " " $ map show $ image

content :: String
content = header ++ body

someFunc :: IO ()
someFunc = do
  writeFile "image.ppm" content
