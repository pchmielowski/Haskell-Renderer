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
    fieldOfView = 30
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

light = (10, 0, -26) :: Vector

data Ray = Ray
  { orig :: Vector
  , direction :: Vector
  }

data Intersection = Intersection
  { point :: Vector
  , normal :: Vector
  }

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

pixel :: Int -> Int -> [Int]
pixel x y = take 3 $ repeat $ color $ closest $ intersections x y spheres
  where
    color intersection =
      case intersection of
        Just it ->
          let intensity = (normal it) .* (lightDirection (point it))
          in max 0 $ round $ intensity * 255
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
