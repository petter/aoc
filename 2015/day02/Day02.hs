module Day02 where

import Data.List
import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse xs = map (fmap read . splitOn "x") $ lines xs

calcSides :: [Int] -> [Int]
calcSides = fmap (uncurry (*)) . sides
  where
    sides [w, h, l] = [(w, h), (w, l), (h, l)]

boxSurfaceArea :: [Int] -> Int
boxSurfaceArea = (2 *) . sum . calcSides

boxSlack :: [Int] -> Int
boxSlack = minimum . calcSides

calcNeededWrappingPaper :: [Int] -> Int
calcNeededWrappingPaper dimensions = (boxSurfaceArea dimensions) + (boxSlack dimensions)

part1 :: [[Int]] -> Int
part1 = sum . map calcNeededWrappingPaper

calcNeededRibbon :: [Int] -> Int
calcNeededRibbon dimensions = (ribbon dimensions) + (bow dimensions)
  where
    ribbon = (2 *) . sum . take 2 . sort
    bow = product

part2 :: [[Int]] -> Int
part2 = sum . map calcNeededRibbon

main :: IO ()
main = do
  contents <- readFile "input.txt"
  boxes <- return $ parse contents
  print $ part1 boxes
  print $ part2 boxes
