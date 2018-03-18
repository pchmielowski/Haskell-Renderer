module Lib
  ( someFunc
  ) where

import Data.List
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
  ]

light = (0, 0, -50) :: Vector

-- returns the distance from ray origin to the intersection point or Nothing
intersection :: Int -> Int -> Sphere -> Maybe Double
intersection x y sphere =
  if tc >= 0 && d2 <= r2
    then Just $
         let t = tc - thc
         in if t > 0
              then t
              else tc + thc
    else Nothing
  where
    tc = l .* (cameraTarget x y) -- tc: vector from ray origin to sphere center projected on the ray
    r2 =
      let r = radius sphere
      in fromIntegral $ r ^ 2
    d2 = norm l ^ 2 - tc ^ 2 -- d2: squared distance from sphere center to ray
    l = (center sphere) <-> cameraSource :: Vector -- l: vector from ray origin to sphere center
    thc = sqrt $ r2 - d2 -- thc: radius projected on the ray

square number = number * number

width = 800

height = 600

header = "P3\n" ++ show (width) ++ " " ++ show (height) ++ "\n255\n"

-- Vec3f phit = rayorig + raydir * tnear; // point of intersection
-- Vec3f nhit = phit - sphere->center; // normal at the intersection point
-- nhit.normalize(); // normalize normal direction
-- Vec3f lightDirection = spheres[i].center - phit;
-- lightDirection.normalize(); 
pixel :: Int -> Int -> [Int]
pixel x y = take 3 $ repeat $ sum $ map color spheres
  where
    color sphere =
      case intersection x y sphere of
        Just t ->
          let q = ((pointHit t) .* (lightDirection t))
          in max 0 $ round $ q * 255
        Nothing -> 0
    pointHit :: Double -> Vector
    pointHit t = cameraSource <+> (cameraTarget x y .^ t)
    normalHit t = normalize $ pointHit t <-> light
    lightDirection t = normalize $ light <-> (pointHit t)

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
