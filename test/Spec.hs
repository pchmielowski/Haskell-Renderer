module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Our Library Tests"
      [ testCase "parseVertex" $
        assertEqual "" (0, 0, 0) $ parseVertex "v 0 0 0"
      , testCase "parseVertex" $
        assertEqual "" (1, 2, 3) $ parseVertex "v 1 2 3"
      ]

parseVertex raw = (parse 0, parse 1, parse 2)
  where
    splitted = (tail . words) raw
    parse = read . (splitted !!)
