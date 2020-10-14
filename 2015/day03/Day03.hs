module Day03 where

import qualified Data.Set as Set

setPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
setPlus (a, b) (x, y) = (a + x, b + y)

direction :: Char -> (Int, Int)
direction '^' = (0, -1)
direction 'v' = (0, 1)
direction '<' = (-1, 0)
direction '>' = (1, 0)

walk :: [Char] -> (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
walk [] _ path = path
walk (c : cs) pos path = walk cs newPos $ Set.insert newPos path
  where
    newPos = setPlus pos (direction c)

part1 :: [Char] -> Int
part1 route = length $ walk route curPos $ Set.fromList [curPos]
  where
    curPos = (0, 0)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents