module Main where

import Data.Either (fromRight)
import Text.Parsec
  ( alphaNum,
    char,
    choice,
    digit,
    many1,
    parse,
    string,
    try,
    ParseError
  )
import Text.Parsec.String (Parser)


parseFile :: String -> [(Int, Int, Char, String)]
parseFile = 
    fromRight [] . traverse (parse lineParser "line parser") . lines
    where 
        lineParser = do
            minN <- fmap read $ many1 digit 
            char '-'
            maxN <- fmap read $ many1 digit 
            char ' '
            c <- alphaNum
            string ": "
            pass <- many1 alphaNum
            return (minN, maxN, c, pass)

part1 :: [(Int, Int, Char, String)] -> Int
part1 = length . filter id . fmap validatePassword
    where
        validatePassword (minN, maxN, c, pass) = cCount <= maxN && cCount >= minN
            where 
                cCount = length $ filter (== c) pass

main :: IO ()
main = do
    file <- readFile "input.txt"
    let passwords = parseFile file 
    print $ part1 passwords
    

