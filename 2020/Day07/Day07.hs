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
    manyTill,
    ParseError,
    (<|>)
  )
import Text.Parsec.Char (space, newline)
import Text.Parsec.String (Parser)
import Data.Either (fromRight)
import Data.List (nub, union)
import qualified Data.Map as Map
import Data.Map ((!))

parser :: String -> Map.Map String [(Int, String)]
parser = Map.fromList . either (error . show) id . parse fileParser "file parser"
    where
        fileParser = lineParser `endBy` newline
        lineParser = do
            rule <- bagNameParser
            space
            string "contain"
            space
            prods <- try none <|> productParser `sepBy` string ", " 
            char '.'
            pure $ (rule, prods)
        word = many1 letter
        bagNameParser = do
            word1 <- word <* space 
            word2 <- word <* space 
            try (string "bags") <|> string "bag"
            return $ word1 ++ " " ++ word2
        productParser = do
            n <- read <$> many1 digit
            space
            bag <- bagNameParser
            pure $ (n, bag)
        none = do
            string "no other bags" 
            return []

allContainedBags :: Map.Map String [(Int, String)] -> String -> [(Int, String)]
allContainedBags bagMap bag = cur `union` childBags
    where 
        cur = bagMap ! bag
        childBags = foldl (\c (_, k) -> c ++ allContainedBags bagMap k) [] cur

flatten :: [[a]] -> [a]
flatten = foldl (++) []

part1 :: Map.Map String [(Int, String)] -> Int
part1 bagMap = length $ filter (\(_, x) -> x == "shiny gold") $ flatten $ allContainedBags bagMap <$> Map.keys bagMap

part2 :: Map.Map String [(Int, String)] -> Int
part2 bagMap = f (bagMap ! "shiny gold")
    where 
        f [] = 0
        f l = sum $ (\(n, ll) -> n + n * f (bagMap ! ll)) <$> l

main :: IO ()
main = do
   file <- readFile "input.txt"
   let bagMap = parser file
   print $ part1 bagMap
   print $ part2 bagMap
