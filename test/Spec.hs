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
        assertEqual "" [(1, 2, 3), (9, 8, 7)] $
        parseVertices "v 1 2 3\nv 9 8 7\n"
      , testCase "parse multiple vertices" $
        assertEqual "" [(1, 2, 3), (9, 8, 7)] $
        parseVertices "v 1 2 3\nv 9 8 7\nf 1 2 3\n l 5 3 1"
      , testCase "parse face" $ assertEqual "" [1, 2, 3] $ parseFace "f 1 2 3"
      , testCase "parse faces" $
        assertEqual "" [[1, 2, 3], [0, 1, 2]] $
        parseFaces "v 0 0 0\nf 1 2 3\nf 0 1 2\n"
      , testCase "parse vertices and face" $
        assertEqual "" [[(1, 2, 3), (9, 8, 7), (0, 0, 0)]] $
        parseTriangles "v 1 2 3\nv 9 8 7\nv 0 0 0\nf 1 2 3"
      , testCase "parse empty file" $ assertEqual "" [] $ parseTriangles ""
      , testCase "parse file with vertices only" $
        assertEqual "" [] $ parseTriangles "v 1 2 3\nv 9 8 7\n"
      , testCase "parse file" $
        assertEqual
          ""
          [ [(0, 0, 0), (1, 1, 1), (0, 0, 1)]
          , [(0, 0, 1), (1, 1, 1), (0, 0, 0)]
          , [(1, 0, 0), (4, 2, 5), (0, 0, 0)]
          , [(1, 1, 1), (0, 0, 1), (4, 2, 5)]
          ] $
        parseTriangles
          "v 0 0 0\n\
          \v 1 1 1\n\
          \v 0 0 1\n\
          \v 1 0 0\n\
          \v 4 2 5\n\
          \f 1 2 3\n\
          \f 3 2 1\n\
          \f 4 5 1\n\
          \f 2 3 5"
      ]

parseVertex :: String -> Vector
parseVertex raw = (parse 0, parse 1, parse 2)
  where
    splitted = (tail . words) raw
    parse = read . (splitted !!)

parseVertices :: String -> [Vector]
parseVertices = map parseVertex . filter isVertex . lines
  where
    isVertex = ('v' ==) . head

type Face = [Int]

parseFace :: String -> Face
parseFace = map read . tail . words

parseFaces :: String -> [Face]
parseFaces = map parseFace . filter isFace . lines
  where
    isFace = ('f' ==) . head

parseTriangles :: String -> [Triangle]
parseTriangles raw = map (toTriangle raw) $ parseFaces raw
  where
    toTriangle :: String -> Face -> Triangle
    toTriangle = map . (. subtract 1) . (!!) . parseVertices
