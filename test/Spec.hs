module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ testCase "parse vertex" $
        assertEqual "" (0, 0, 0) $ parseVertex "v 0 0 0"
      , testCase "parse vertex" $
        assertEqual "" (0.123, 0.234, 0.345) $
        parseVertex "v 0.123 0.234 0.345 1.0"
      , testCase "parse multiple vertices" $
        assertEqual "" [] $ parseVertices ""
      , testCase "parse multiple vertices" $
        assertEqual "" [(1, 2, 3), (9, 8, 7)] $ parseVertices "v 1 2 3\nv 9 8 7\n"
      ]

parseVertex :: String -> Vector
parseVertex raw = (parse 0, parse 1, parse 2)
  where
    splitted = (tail . words) raw
    parse = read . (splitted !!)

parseVertices :: String -> [Vector]
parseVertices raw = map parseVertex $ lines raw
