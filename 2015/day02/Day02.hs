module Day01 where

import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse xs = map (fmap read . splitOn "x") $ lines xs

sides :: [Int] -> [(Int, Int)]
sides [w, h, l] = [(w, h), (w, l), (h, l)]

calcSides :: [Int] -> [Int]
calcSides = fmap (uncurry (*)) . sides

boxSurfaceArea :: [Int] -> Int
boxSurfaceArea = (2 *) . sum . calcSides

boxSlack :: [Int] -> Int
boxSlack = minimum . calcSides

calcNeededWrappingPaper :: [Int] -> Int
calcNeededWrappingPaper dimensions = (boxSurfaceArea dimensions) + (boxSlack dimensions)

calcBoxes :: [[Int]] -> Int
calcBoxes = sum . map calcNeededWrappingPaper

main :: IO ()
main = do
  contents <- readFile "input.txt"
  boxes <- return $ parse contents
  putStrLn $ show $ calcBoxes boxes