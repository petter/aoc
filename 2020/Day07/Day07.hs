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

allContainedBags :: String -> Map.Map String [(Int, String)] -> [String]
allContainedBags bag bagMap = fmap snd cur `union` childBags
    where 
        cur = bagMap ! bag
        childBags :: [String]
        childBags = foldl (\c (_, k) -> c `union` allContainedBags k bagMap) [] cur

part1 :: Map.Map String [(Int, String)] -> Int
part1 bagMap = length $ filter (elem "shiny gold") $ fmap ((flip $ allContainedBags) bagMap) $ Map.keys bagMap

main :: IO ()
main = do
   file <- readFile "input.txt"
   let bagMap = parser file
   print $ part1 bagMap
