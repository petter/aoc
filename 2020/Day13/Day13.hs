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
import Data.List.Index
import Control.Monad (zipWithM)

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

isCoprime :: [Maybe Int] -> Bool
isCoprime buses = all (==1) $ fmap (uncurry gcd) pairs
    where
        pairs = [(x, y) | x <- xs, y <- xs, x < y]
        xs = catMaybes buses

-- brute force
-- part2 :: [Maybe Int] -> Maybe Int
-- part2 buses = lowestTimestamp maxId
--     where
--         lowestTimestamp (i, k) = (\n -> n - i) <$> find (\n -> check (n - i)) [0,k..] 
--         check n = all (\(i, busId) -> ((i + n) `mod` busId) == 0) indexedPairs
--         maxId = maximumBy (\(_,a) (_,b) -> compare a b) $ indexedPairs 
--         indexedPairs = fmap (\(i, v) -> (i, fromJust v)) $ filter (isJust . snd) $ indexed $filter isJust buses

-- Attempt at CRT
-- part2 buses = (sum $ fmap (\v -> ai v * _Ni v * xi v) indexedPairs) `mod` _N
--     where
--         indexedPairs = fmap (\(i, v) -> (i, fromJust v)) $ filter (isJust . snd) $ indexed buses
--         _N = product $ snd <$> indexedPairs
--         _Ni (_, ni) = round $ fromIntegral _N / fromIntegral ni
--         xi (_, ni) = round $ (fromIntegral ni / fromIntegral _N) `mod` fromIntegral _N
--         ai (i, ni) = ni - (i `mod` ni) 

-- From https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
      (s, t) = egcd b r
      (q, r) = a `quotRem` b
           
modInv :: Int -> Int -> Either String Int
modInv a b =
    case egcd a b of
        (x, y)
            | a * x + b * y == 1 -> Right x
            | otherwise ->
                Left $ "No modular inverse for " ++ show a ++ " and " ++ show b
                          
chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
    zipWithM modInv crtModulii modulii >>=
    (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
    where
        modPI = product modulii
        crtModulii = (modPI `div`) <$> modulii

part2 :: [Maybe Int] -> Either String Int
part2 buses = chineseRemainder residues modulii
    where
        residues = (\(i, v) -> (v - (i `mod` v)) `mod` v) <$> indexedPairs
        modulii = snd <$> indexedPairs
        indexedPairs = fmap (\(i, v) -> (i, fromJust v)) $ filter (isJust . snd) $ indexed buses

main :: IO ()
main = do
    (_, testBuses) <- parser <$> readFile "testinput.txt"
    (timestamp, buses) <- parser <$> readFile "input.txt"
    print $ part1 timestamp buses
    print $isCoprime buses
    print $ part2 testBuses
    print $ part2 buses
    


