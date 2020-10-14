module Day03 where

import Control.Lens (indices, toListOf, traversed)
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

part2 :: [Char] -> Int
part2 route = length $ Set.union santaPath robotPath
  where
    startPos = (0, 0)
    walkRoute instructions = walk instructions startPos $ Set.fromList [startPos]
    santaInstructions = toListOf (traversed . indices even) route
    robotInstructions = toListOf (traversed . indices odd) route
    santaPath = walkRoute santaInstructions
    robotPath = walkRoute robotInstructions

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents