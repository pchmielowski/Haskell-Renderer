module Lib
  ( someFunc
  ) where

import Data.List

width = 300

height = 200

header = "P3\n" ++ show (width) ++ " " ++ show (height) ++ "\n255\n"

color x y = show (x + y) ++ " 0 0"

rows = map (\y -> intercalate " " $ map (\x -> color x y) [1..width]) [1..height]

body = intercalate " " rows

content = header ++ body

someFunc :: IO ()
someFunc = do
  writeFile "image.ppm" content
