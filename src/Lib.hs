module Lib
  ( someFunc
  ) where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Vec3

type Vector = TVec3

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
light = (10, 0, 2)

data Ray = Ray
  { orig :: Vector
  , direction :: Vector
  }

data Intersection = Intersection
  { point :: Vector
  , normal :: Vector
  }

type Triangle = [Vector]

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

intersections :: Int -> Int -> [Intersection]
intersections x y = catMaybes $ map (intersection (cameraRay x y)) triangles
  where
    triangles = concat [cone, ground]
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
      [ [(-2, -2, z0), (2, 2, z0), (-2, 2, z0)]
      , [(-2, -2, z0), (2, -2, z0), (2, 2, z0)]
      ]
    r = sqrt 2
    z1 = 0
    z0 = -2

pixel :: Int -> Int -> [Int]
pixel x y = take 3 $ repeat $ color $ closest $ intersections x y
  where
    color intersection =
      case intersection of
        Just it ->
          let intensity = (normal it) .* (lightDirection light (point it))
          in max 0 $ 128 + (round $ intensity * 128)
        Nothing -> 0
    lightDirection = (normalize .) . (<->)

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
