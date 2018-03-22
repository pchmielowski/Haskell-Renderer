module Reader where

import Lib

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
