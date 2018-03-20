module Lib
  ( someFunc
  ) where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Vec3

type Vector = TVec3

cameraSource :: Vector
cameraSource = (0, 0, 0)

cameraTarget :: Int -> Int -> Vector
cameraTarget x y =
  normalize
    ( (2 * ((x' + 0.5) / width') - 1) * angle * width' / height'
    , (1 - 2 * (y' + 0.5) / height') * angle
    , -1)
  where
    angle = tan $ pi * 0.5 * fieldOfView / 180
    fieldOfView = 5
    x' = fromIntegral x
    y' = fromIntegral y
    width' = fromIntegral width
    height' = fromIntegral height

data Sphere = Sphere
  { center :: Vector
  , radius :: Int
  }

spheres =
  [ Sphere (3, 3, -30) 1
  , Sphere (-3, 3, -30) 2
  , Sphere (3, -3, -40) 1
  , Sphere (-3, -3, -30) 3
  , Sphere (-3, -3, -40) 1 -- should be hidden behind the previous one
  ]

light = (0, 8, -20) :: Vector

data Ray = Ray
  { orig :: Vector
  , direction :: Vector
  }

data Intersection = Intersection
  { point :: Vector
  , normal :: Vector
  }

type Triangle = [Vector]

triangleIntersection :: Ray -> Triangle -> Maybe Intersection
triangleIntersection ray triangle =
  if a > -eps && a < eps
    then Nothing
    else if u < 0 || u > 1
           then Nothing
           else if v < 0 || u + v > 1
                  then Nothing
                  else if t > eps
                         then Just $ Intersection point normal
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
    point = ((orig ray) <+> (direction ray) .^ t)
    normal = edge1 >< edge2

intersection :: Ray -> Sphere -> Maybe Intersection
intersection ray sphere =
  if tc >= 0 && d2 <= r2
    then Just $
         let point = pointAt t
         in Intersection point $ normalize $ point <-> (center sphere)
    else Nothing
  where
    t =
      let t0 = tc - thc
      in if t0 > 0
           then t0
           else tc + thc
    tc = l .* (direction ray) -- tc: vector from ray orig to sphere center projected on the ray
    r2 =
      let r = radius sphere
      in fromIntegral $ r ^ 2
    d2 = norm l ^ 2 - tc ^ 2 -- d2: squared distance from sphere center to ray
    l = (center sphere) <-> (orig ray) :: Vector -- l: vector from ray orig to sphere center
    thc = sqrt $ r2 - d2 -- thc: radius projected on the ray
    pointAt t = (orig ray) <+> (direction ray .^ t)

square number = number * number

width = 800

height = 600

header = "P3\n" ++ show (width) ++ " " ++ show (height) ++ "\n255\n"

cameraRay x y = Ray cameraSource $ cameraTarget x y

closest :: [Intersection] -> Maybe Intersection
closest [] = Nothing
closest intersections =
  Just $ minimumBy (comparing distanceFromOrigin) intersections
  where
    distanceFromOrigin intersection =
      norm (cameraSource <-> (point intersection))

intersections :: Int -> Int -> [Sphere] -> [Intersection]
intersections x y spheres =
  catMaybes $ map (intersection (cameraRay x y)) spheres

triangleIntersections x y =
  catMaybes $ map (triangleIntersection (cameraRay x y)) triangles
  where
    triangles =
      [ [(0, 0, z1), (-1, -1, z0), (0, -r, z0)]
      , [(0, 0, z1), (0, -r, z0), (1, -1, z0)]
      , [(0, 0, z1), (1, -1, z0), (r, 0, z0)]
      , [(0, 0, z1), (r, 0, z0), (1, 1, z0)]
      , [(0, 0, z1), (1, 1, z0), (0, r, z0)]
      , [(0, 0, z1), (0, r, z0), (-1, 1, z0)]
      , [(0, 0, z1), (-1, 1, z0), (-r, 0, z0)]
      , [(0, 0, z1), (-r, 0, z0), (-1, -1, z0)]
      ]
    r = sqrt 2
    z1 = -30
    z0 = z1 - h
    h = 10

pixel :: Int -> Int -> [Int]
pixel x y = take 3 $ repeat $ color $ closest $ triangleIntersections x y
  where
    color intersection =
      case intersection of
        Just it ->
          let intensity = (normal it) .* (lightDirection (point it))
          in max 0 $ 128 + (round $ intensity * 128)
        Nothing -> 0
    lightDirection point = normalize $ light <-> point

row :: Int -> [Int]
row y = concat $ map (\x -> pixel x y) [1 .. width]

image :: [Int]
image = concat $ map row [1 .. height]

body :: String
body = intercalate " " $ map show $ image

content = header ++ body

someFunc :: IO ()
someFunc = do
  writeFile "image.ppm" content
