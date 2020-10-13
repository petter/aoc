module Day01 where

import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse xs = map (fmap read . splitOn "x") $ lines xs

sides :: [Int] -> [(Int, Int)]
sides [w, h, l] = [(w, h), (w, l), (h, l)]

calcSides :: [Int] -> [Int]
calcSides dimensions = fmap (uncurry (*)) $ sides dimensions

boxSurfaceArea :: [Int] -> Int
boxSurfaceArea dimensions = (2 *) $ sum $ calcSides dimensions

boxSlack :: [Int] -> Int
boxSlack dimensions = minimum $ calcSides dimensions

calcNeededWrappingPaper :: [Int] -> Int
calcNeededWrappingPaper dimensions = (boxSurfaceArea dimensions) + (boxSlack dimensions)

calcBoxes :: [[Int]] -> Int
calcBoxes boxes = sum $ map calcNeededWrappingPaper boxes

main :: IO ()
main = do
  contents <- readFile "input.txt"
  boxes <- return $ parse contents
  putStrLn $ show $ calcBoxes boxes