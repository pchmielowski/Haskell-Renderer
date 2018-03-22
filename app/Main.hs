module Main where

import Lib

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  obj <- readFile $ args !! 0
  writeFile "image.ppm" $ content $ parseTriangles obj
