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
import Data.List (nub, intersect)

data Rule = Rule String [Product]
    deriving (Show)
data Product = Product Int String
    deriving (Show)

parser :: String -> [Rule]
parser = either (error . show) id . parse fileParser "file parser"
    where
        fileParser = lineParser `endBy` newline
        lineParser = do
            rule <- bagNameParser
            space
            string "contain"
            space
            prods <- try none <|> productParser `sepBy` string ", " 
            char '.'
            pure $ Rule rule prods
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
            pure $ Product n bag
        none = do
            string "no other bags" 
            return []

main :: IO ()
main = do
   file <- readFile "input.txt"
   let rules = parser file
   print rules
