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

data Bus = Bus Int
         | OutOfService
         deriving (Show, Eq)

parser :: String -> (Int, [Bus])
parser = either (error . show) id . parse fileParser "file parser"
    where
        fileParser = do
            timestamp <- read <$> many1 digit
            newline
            buses <- busParser `sepBy` char ','
            return (timestamp, buses)

        busParser = do
            busId <- try (string "x") <|> many1 digit
            return $ case busId of "x" -> OutOfService
                                   n  -> Bus (read n)

part1 :: Int -> [Bus] -> Int
part1 timestamp buses = (\(busId, busTimestamp) -> busId * (busTimestamp - timestamp)) $ minimumBy (\(_, a) (_, b) -> compare a b) $ firstLargerTimestamp <$> onlyValidBuses
    where
        onlyValidBuses = filter (/= OutOfService) buses
        busToList (Bus id) = [0,id..]
        busToList (OutOfService) = []
        firstLargerTimestamp (Bus busId) = (busId, fromJust $ find (>= timestamp) (busToList (Bus busId)))
        firstLargerTimestamp OutOfService = (-1, -1)

main :: IO ()
main = do
    (timestamp, buses) <- parser <$> readFile "input.txt"
    print timestamp
    print $ part1 timestamp buses
    


