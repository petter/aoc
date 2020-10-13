module Day01 where

import Data.List

parseFloor :: Char -> Int
parseFloor '(' = 1
parseFloor ')' = (-1)

floorParser :: [Char] -> Int
floorParser = sum . fmap parseFloor

floorParser2 :: [Char] -> Maybe Int
floorParser2 = findIndex (== -1) . scanl (+) 0 . fmap parseFloor

main :: IO ()
main = do
  contents <- readFile "day01.txt"
  print $ floorParser contents
  print $ floorParser2 contents