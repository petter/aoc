module Main where

import Text.Parsec
  ( alphaNum,
    letter,
    char,
    choice,
    count,
    digit,
    many1,
    skipMany1,
    parse,
    string,
    try,
    sepBy,
    endBy,
    ParseError,
    (<|>)
  )
import Text.Parsec.Char (newline, oneOf)
import Data.Either (fromRight)
import Data.List (nub, intersect, groupBy, sortBy)

parser :: String -> [(String, String)]
parser = fromRight (error "bad") . parse fileParser "file parser"
    where
        fileParser = lineParser `endBy` newline 
        lineParser = do
            row <- count 7 $ oneOf "FB"
            column <- count 3 $ oneOf "LR"
            return (row, column)

seat :: (String, String) -> (Int, Int)
seat (row, col) = (rowParser row, colParser col)
    where 
        takeOrDrop 'F' = take
        takeOrDrop 'L' = take
        takeOrDrop 'B' = drop
        takeOrDrop 'R' = drop
        rowParser = head . foldl (\l f -> f ((length l) `div` 2) l) [0..127] . fmap takeOrDrop
        colParser = head . foldl (\l f -> f ((length l) `div` 2) l) [0..8] . fmap takeOrDrop

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

part1 :: [(String, String)] -> Int
part1 = maximum . fmap (seatId . seat)

part2 :: [(String, String)] -> [[(Int, Int)]]
part2 = filter ((/= 8) . length) . groupBy (\(a,_) (b,_) -> a == b) . sortBy (\(a,_) (b,_) -> compare a b) . fmap seat 


main :: IO ()
main = do
    file <- readFile "input.txt"
    let seats = parser file 
    print $ part1 seats
    print $ part2 seats
    print $ seatId (90, 7)
