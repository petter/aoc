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
import Text.Parsec.Char (space, newline, oneOf)
import Text.Parsec.String (Parser)
import Data.List (nub, union)
import qualified Data.Set as S


data Op = Acc Int
        | Jmp Int
        | Nop Int
        deriving (Show, Ord, Eq)

parser :: String -> [Op]
parser = either (error . show) id . traverse (parse lineParser "line parser") . lines
    where
        lineParser = do
            opText <- try (string "acc") <|> try (string "jmp") <|> string "nop"
            space
            digitPrefix <- oneOf "+-"
            num <- read <$> many1 digit

            let op = case opText of "acc" -> Acc
                                    "jmp" -> Jmp
                                    "nop" -> Nop
            let numWithPrefix = case digitPrefix of '+' -> num
                                                    '-' -> -num
            return $ op numWithPrefix

            
part1 :: [Op] -> Int
part1 ops = f 0 0 S.empty
    where 
        f ip acc visited = if hasVisitedNextIp then nextAcc else f nextIp nextAcc (S.insert ip visited)
            where 
                curOp = ops !! ip
                (nextAcc, nextIp) = case curOp of (Jmp i) -> (acc, ip + i)
                                                  (Acc i) -> (acc + i, ip + 1) 
                                                  _ -> (acc, ip + 1)
                hasVisitedNextIp = S.member nextIp visited


main :: IO ()
main = do
    file <- readFile "input.txt"
    let ops = parser file
    print $ part1 ops
