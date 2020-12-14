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
import Data.Bits ((.&.), (.|.))
import qualified Data.Map as M

data Op = WriteOp Int Int
        | NewMask String
        deriving (Show)

parser :: String -> [Op]
parser = either (error . show) id . traverse (parse lineParser "line parser") . lines
    where
        lineParser = try maskParser <|> writeParser
        maskParser = do 
            string "mask = "
            mask <- many1 $ oneOf "X10"
            return $ NewMask mask
        writeParser = do
            string "mem["
            address <- read <$> many1 digit
            string "] = "
            num <- read <$> many1 digit
            return $ WriteOp address num

applyMask :: String -> Int -> Int
applyMask = rec 35
    where
        rec _ [] num = num
        rec n (c:cs) num = case c of 'X' -> rec (n - 1) cs num
                                     '1' -> rec (n - 1) cs (num .|. (2^n))
                                     '0' -> rec (n - 1) cs (num .&. (maxNum - 2^n))
        maxNum = foldl (\s c -> s + (2 ^ c)) 0 [0..35]

getAddresses :: String -> Int -> [Int]
getAddresses = rec [] 35
    where
        rec res _ [] address = (address:res)
        rec res n ('0':cs) address = rec res (n-1) cs address
        rec res n ('1':cs) address = rec res (n-1) cs (address .|. (2^n))
        rec res n ('2':cs) address = rec res (n-1) cs (address .&. (maxNum - 2^n)) -- Replace with 0
        rec res n ('X':cs) address = rec res n ('1':cs) address ++ rec res n ('2':cs) address
        maxNum = foldl (\s c -> s + (2 ^ c)) 0 [0..35]
    
            
part1 :: [Op] -> Int
part1 = doOp M.empty "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    where
        doOp memory _ [] = foldl (+) 0 memory
        doOp memory mask ((NewMask s):ops) = doOp memory s ops
        doOp memory mask ((WriteOp address value):ops) = doOp (M.insert address (applyMask mask value) memory) mask ops

part2 :: [Op] -> Int
part2 = doOp M.empty "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    where
        doOp memory _ [] = foldl (+) 0 memory
        doOp memory mask ((NewMask s):ops) = doOp memory s ops
        doOp memory mask ((WriteOp address value):ops) = doOp (foldl (\newMem addr -> M.insert addr value newMem) memory (getAddresses mask address)) mask ops

main :: IO ()
main = do
    input <- parser <$> readFile "input.txt"
    print $ part1 input
    print $ part2 input
