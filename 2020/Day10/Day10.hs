module Main where

import Data.List

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (x ==) xs

part1 :: [Int] -> Int
part1 nums = (1 + count diffs 1) * (1 + count diffs 3)
    where
        sortedNums = sort nums
        zipWithNext = zip <*> tail
        diffs = uncurry subtract <$> zipWithNext sortedNums

main :: IO ()
main = do
    file <- readFile "input.txt"
    let input = read <$> lines file
    print $ part1 input
