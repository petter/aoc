module Main where

import Data.List

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (x ==) xs

part1 :: [Int] -> Int
part1 nums = count diffs 1 * count diffs 3
    where
        zipWithNext = zip <*> tail
        diffs = uncurry subtract <$> zipWithNext nums

isValid nums = all (<= 3) diffs
    where
        zipWithNext = zip <*> tail
        diffs = uncurry subtract <$> zipWithNext nums


part2 (first:nums) = product ((\(chunkHead:chunkRest) -> combinations [chunkHead] chunkRest) <$> chunks)
    where 
        combinations _ [] = 1
        combinations before (n:[]) = if isValid (before ++ [n]) then 1 else 0
        combinations before (n:after) = 
            if isValid (before ++ (n:after)) then 
                combinations before after + combinations (before ++ [n]) after
            else 
                0
        chunks = chunk [] [first] nums
        chunk result cur (n:[]) = result ++ [(cur ++ [n])]
        chunk result cur (n:ns) = 
            if (n - last cur) == 3 then
                chunk (result ++ [cur]) [n] ns
            else 
                chunk result (cur ++ [n]) ns

main :: IO ()
main = do
    let fileNames = ["simpletest.txt", "testinput.txt", "input.txt"]
    files <- traverse readFile fileNames
    let inputs = zip fileNames $ (sort . (\nums -> nums ++ [0, maximum nums + 3]) . (fmap read) . lines) <$> files

    print "Part 1"
    print $ (\(a, b) -> (a, part1 b)) <$> inputs

    print "Part 2"
    print $ (\(a, b) -> (a, part2 b)) <$> inputs
