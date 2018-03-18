module Lib
  ( someFunc
  ) where

import Data.List
import Data.Vec3

cameraSource :: TVec3
cameraSource = (0, 0, 0)

-- float angle = tan(M_PI * 0.5 * fov / 180.);
-- float xx = (2 * ((x + 0.5) * invWidth) - 1) * angle * aspectratio;
-- float yy = (1 - 2 * ((y + 0.5) * invHeight)) * angle;
-- Vec3f raydir(xx, yy, -1);
-- raydir.normalize(); 
cameraTarget :: Int -> Int -> TVec3
cameraTarget x y =
  normalize
    ( (2 * ((xf + 0.5) / wf) - 1) * angle * wf / hf
    , (1 - 2 * (yf + 0.5) / hf) * angle
    , -1)
  where
    angle = tan $ pi * 0.5 * fieldOfView / 180
    fieldOfView = 30
    -- TODO find better way to handle Int -> Double conversion
    xf = fromIntegral x
    yf = fromIntegral y
    wf = fromIntegral width
    hf = fromIntegral height

data Sphere = Sphere
  { center :: TVec3
  , radius :: Int
  }

sphere = Sphere (0, 0, -30) 1

-- Vec3f l = center - rayorig;
-- float tca = l.dot(raydir);
-- if (tca < 0) return false;
-- float d2 = l.dot(l) - tca * tca;
-- if (d2 > radius2) return false;
-- float thc = sqrt(radius2 - d2);
-- t0 = tca - thc;
-- t1 = tca + thc;
-- return true; 
isPoint x y = tca >= 0 && d2 <= r2
  where
    tca = l .* (cameraTarget x y)
    r2 = fromIntegral ((radius sphere) * (radius sphere))
    d2 = l .* l - (tca * tca)
    l = (center sphere) <-> cameraSource :: TVec3

square number = number * number

width = 800

height = 600

header = "P3\n" ++ show (width) ++ " " ++ show (height) ++ "\n255\n"

pixel :: Int -> Int -> [Int]
pixel x y = [color, color, color]
  where
    color =
      if (isPoint x y)
        then 255
        else 0

row :: Int -> [Int]
row y = concat $ map (pixel y) [1 .. width]

image :: [Int]
image = concat $ map row [1 .. height]

body :: String
body = intercalate " " $ map show $ image

content = header ++ body

someFunc :: IO ()
someFunc = do
  writeFile "image.ppm" content
