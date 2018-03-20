module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [add5Test])

add5Test :: TestTree
add5Test =
  testCase "Testing add5" (assertEqual "Should add 5 to get 10" 10 (add5 5))

