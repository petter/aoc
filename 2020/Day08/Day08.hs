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
import Data.List.Index (setAt)


data Op = Acc Int
        | Jmp Int
        | Nop Int
        deriving (Show, Ord, Eq)

data ExitStatus = Terminated Int
                | InifiniteLoop Int
                deriving (Show)

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

runProgram :: [Op] -> ExitStatus
runProgram ops = f 0 0 S.empty
    where 
        f ip acc visited 
            | shouldTerminate = Terminated nextAcc
            | hasVisitedNextIp = InifiniteLoop nextAcc
            | otherwise = f nextIp nextAcc (S.insert ip visited)
            where 
                curOp = ops !! ip
                (nextAcc, nextIp) = case curOp of (Jmp i) -> (acc, ip + i)
                                                  (Acc i) -> (acc + i, ip + 1) 
                                                  _ -> (acc, ip + 1)
                hasVisitedNextIp = S.member nextIp visited
                shouldTerminate = nextIp >= length ops

part1 :: [Op] -> ExitStatus 
part1 = runProgram

part2 :: [Op] -> Maybe ExitStatus
part2 ops = f 0
    where
        f i
          | i >= length ops = Nothing
          | changeOp = case runProgram (changeInstruction i) of (Terminated i) -> Just (Terminated i)
                                                                otherwise -> f (i + 1)
          | otherwise = f (i + 1)
          where 
            changeOp = case ops !! i of (Jmp _) -> True
                                        (Nop _) -> True
                                        otherwise -> False
        changeInstruction i = setAt i nextOp ops
            where 
                curOp = ops !! i
                nextOp = flipOp curOp
        flipOp (Jmp i) = Nop i
        flipOp (Nop i) = Jmp i

main :: IO ()
main = do
    file <- readFile "input.txt"
    let ops = parser file
    print $ part1 ops
    print $ part2 ops
