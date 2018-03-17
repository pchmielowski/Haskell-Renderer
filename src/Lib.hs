module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do 
    writeFile "image.ppm" "P3\n2 2\n255\n255 0 0 255 0 255\n0 0 0 0 255 0\n"
