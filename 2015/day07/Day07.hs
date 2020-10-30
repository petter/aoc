module Main where

import Text.Parsec
import Text.Parsec.String (Parser)

data Input 
    = Value Int
    | Input String
    deriving (Show)

data Wire
    = And Input Input
    | Or Input Input
    | RShift Input Input
    | LShift Input Input
    | Not Input
    | Id Input
    deriving (Show)

number :: Parser Input
number = Value . read <$> many1 digit

word :: Parser Input
word = Input <$> many1 letter

parseInput :: Parser Input
parseInput = do
    try number <|> word

parseId :: Parser Wire
parseId = Id <$> parseInput

parseNot :: Parser Wire
parseNot = do 
    string "NOT "
    Not <$> parseInput

parseBinOp :: Parser Wire
parseBinOp = do
    val1 <- parseInput
    opString <- choice $ fmap (try . string) [" AND ", " OR ", " RSHIFT ", " LSHIFT "]
    op <- return $ case opString of
        " AND " -> And
        " OR " -> And
        " RSHIFT " -> RShift
        " LSHIFT " -> LShift
        _ -> error "invalid value"
    val2 <- parseInput

    return $ op val1 val2

parseAssignment :: Parser (String, Wire)
parseAssignment = do
    wire <- try parseNot <|> try parseBinOp <|> parseId
    string " -> "
    assignedTo <- many1 letter
    return $ (assignedTo, wire) 
    
    
main :: IO ()
main = do
  contents <- readFile "input.txt"
  test <- return $ fmap (parse parseAssignment "") $ lines contents
  print test
