{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (find)

part1and2 :: [[Int]] -> Maybe Int
part1and2 input = 
    prodMaybePair sumPair
    where
        prodMaybePair = fmap product 
        sumPair = find (sumEqual2020) input
        sumEqual2020 p = (sum p) == 2020

part1 :: [Int] -> Maybe Int
part1 input = 
    part1and2 $ cartProd input input
    where
        cartProd xs ys = [[x,y] | x <- xs, y <- ys]
        
part2 :: [Int] -> Maybe Int
part2 input = 
    part1and2 $ cartProd input input input
    where
        cartProd xs ys zs = [[x,y,z] | x <- xs, y <- ys, z <- zs]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input :: [Int] = fmap read $ lines content 
    print $ part1 input
    print $ part2 input
