module Lib
  ( someFunc
  ) where

import Data.List

width = 300

height = 200

header = "P3\n" ++ show (width) ++ " " ++ show (height) ++ "\n255\n"

pixel :: Int -> Int -> [Int]
pixel x y = [x + y, x, y]

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
