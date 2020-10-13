module Day01 where

import Data.List
import Data.Monoid

parseFloor :: Char -> Int
parseFloor '(' = 1
parseFloor ')' = (-1)

floorParser :: [Char] -> Int
floorParser = sum . fmap parseFloor

floorParser2 :: [Char] -> Maybe Int
floorParser2 = findIndex (== -1) . scanl (+) 0 . fmap parseFloor

--- part 1 alt
f '(' = Sum 1
f ')' = Sum (-1)

main :: IO ()
main = do
  contents <- readFile "day01.txt"
  print $ floorParser contents
  print $ floorParser2 contents

  -- part 1 alt
  print $ foldMap f contents