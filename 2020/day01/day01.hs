{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (find)

part1 :: [Int] -> Maybe Int
part1 input = 
    prodMaybePair sumPair
    where
        prodMaybePair = fmap (uncurry (*))
        sumPair = find (sumEqual2020) $ cartProd input input
        sumEqual2020 p = ((uncurry (+)) p) == 2020
        cartProd xs ys = [(x,y) | x <- xs, y <- ys]
        
    

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input :: [Int] = fmap read $ lines content 
    print $ part1 input
