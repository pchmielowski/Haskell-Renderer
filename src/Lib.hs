module Lib
  ( someFunc
  ) where

import Data.List
import Data.Vec3

type Vector = TVec3

cameraSource :: Vector
cameraSource = (0, 0, 0)

-- float angle = tan(M_PI * 0.5 * fov / 180.);
-- float xx = (2 * ((x + 0.5) * invWidth) - 1) * angle * aspectratio;
-- float yy = (1 - 2 * ((y + 0.5) * invHeight)) * angle;
-- Vec3f raydir(xx, yy, -1);
-- raydir.normalize(); 
cameraTarget :: Int -> Int -> Vector
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
  { center :: Vector
  , radius :: Int
  }

sphere = Sphere (0, -5, -30) 1

light = (30, 20, -10) :: Vector

-- Vec3f l = center - rayorig;
-- float tca = l.dot(raydir);
-- if (tca < 0) return false;
-- float d2 = l.dot(l) - tca * tca;
-- if (d2 > radius2) return false;
-- float thc = sqrt(radius2 - d2);
-- t0 = tca - thc;
-- t1 = tca + thc;
-- return true;
isPoint :: Int -> Int -> Maybe (Double, Double)
isPoint x y =
  if tca >= 0 && d2 <= r2
    then Just (t0, t1)
    else Nothing
  where
    tca = l .* (cameraTarget x y)
    r2 =
      let r = radius sphere
      in fromIntegral $ r * r
    d2 = l .* l - (tca * tca)
    l = (center sphere) <-> cameraSource :: Vector
    thc = sqrt $ r2 - d2
    t0 = tca - thc
    t1 = tca + thc

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
pixel x y = [color, color, color]
  where
    color =
      case (isPoint x y) of
        Just (t0, _) ->
          let q = -((pointHit t0) .* (lightDirection t0))
          in max 0 $ round $ q * 255
        Nothing -> 0
    pointHit :: Double -> Vector
    pointHit t0 = cameraSource <+> (cameraTarget x y .^ t0)
    normalHit t0 = normalize $ pointHit t0 <-> light
    lightDirection t0 = normalize $ light <-> (pointHit t0)

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
