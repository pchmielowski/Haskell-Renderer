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

light = (0, 0, -30) :: Vector

data Ray = Ray
  { orig :: Vector
  , direction :: Vector
  }

-- returns the intersection point or Nothing
intersection :: Ray -> Sphere -> Maybe Vector
intersection ray sphere =
  if tc >= 0 && d2 <= r2
    then Just $
         pointAt $
         let t = tc - thc
         in if t > 0
              then t
              else tc + thc
    else Nothing
  where
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

pixel :: Int -> Int -> [Int]
pixel x y = take 3 $ repeat $ sum $ map color spheres
  where
    color sphere =
      case intersection (cameraRay x y) sphere of
        Just point ->
          let intensity = normal point (center sphere) .* (lightDirection point)
          in max 0 $ round $ intensity * 255
        Nothing -> 0
    normal point c = normalize $ point <-> c
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
