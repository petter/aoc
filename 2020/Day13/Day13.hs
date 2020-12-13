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
import Data.List
import Data.Maybe

parser :: String -> (Int, [Maybe Int])
parser = either (error . show) id . parse fileParser "file parser"
    where
        fileParser = do
            timestamp <- read <$> many1 digit
            newline
            buses <- busParser `sepBy` char ','
            return (timestamp, buses)

        busParser = do
            busId <- try (string "x") <|> many1 digit
            return $ case busId of "x" -> Nothing
                                   n  -> Just (read n)

part1 :: Int -> [Maybe Int] -> Int
part1 timestamp = res . minimumBy (\(_, a) (_, b) -> compare a b) . fmap firstLargerTimestamp . catMaybes 
    where
        res (busId, busTimestamp) = busId * (busTimestamp - timestamp)
        busToList busId = [0,busId..]
        firstLargerTimestamp busId = (busId, fromJust $ find (>= timestamp) (busToList busId))

main :: IO ()
main = do
    (timestamp, buses) <- parser <$> readFile "input.txt"
    print timestamp
    print $ part1 timestamp buses
    


