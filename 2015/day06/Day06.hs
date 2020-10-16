module Main where

import Data.Either (fromRight)
import qualified Data.Map as Map
import Text.Parsec
  ( alphaNum,
    char,
    choice,
    digit,
    many1,
    parse,
    string,
    try,
  )
import Text.Parsec.String (Parser)

data Coord = Coord Int Int
  deriving (Eq, Show, Ord)

data Action
  = TurnOn Coord Coord
  | TurnOff Coord Coord
  | Toggle Coord Coord
  deriving (Eq, Show)

coordParser :: Parser Coord
coordParser = do
  firstNum <- many1 digit
  char ','
  secondNum <- many1 digit
  return $ Coord (read firstNum) (read secondNum)

actionParser :: Parser Action
actionParser = do
  actionString <- choice [try $ string "turn on", try $ string "turn off", string "toggle"]
  action <- return $ case actionString of
    "turn on" -> TurnOn
    "turn off" -> TurnOff
    "toggle" -> Toggle

  char ' '
  firstCoord <- coordParser
  string " through "
  secondCoord <- coordParser

  return $ action firstCoord secondCoord

wordParser :: Parser String
wordParser = do
  w <- many1 alphaNum
  return w

doActions1 :: [Action] -> Map.Map Coord Int
doActions1 = foldl doAction Map.empty
  where
    genBox :: Coord -> Coord -> [Coord]
    genBox (Coord x1 y1) (Coord x2 y2) = [Coord a b | a <- [x1 .. x2], b <- [y1 .. y2]]
    doActionHelper :: (Int -> Int -> Int) -> Int -> Map.Map Coord Int -> Coord -> Coord -> Map.Map Coord Int
    doActionHelper f n mp c1 c2 = foldr (flip (Map.insertWith f) n) mp $ genBox c1 c2
    doAction :: Map.Map Coord Int -> Action -> Map.Map Coord Int
    doAction mp (TurnOn c1 c2) = doActionHelper const 1 mp c1 c2
    doAction mp (TurnOff c1 c2) = doActionHelper const (-1) mp c1 c2
    doAction mp (Toggle c1 c2) = doActionHelper ((*) . negate) 1 mp c1 c2

part1 = length . Map.filter (== 1) . doActions1

doActions2 :: [Action] -> Map.Map Coord Int
doActions2 = foldl doAction startMap
  where
    startMap = Map.fromList $ fmap (flip (,) 0) $ genBox (Coord 0 0) (Coord 999 999)
    genBox (Coord x1 y1) (Coord x2 y2) = [Coord a b | a <- [x1 .. x2], b <- [y1 .. y2]]
    doActionHelper f n mp c1 c2 = foldr (flip (Map.insertWith f) n) mp $ genBox c1 c2
    doAction mp (TurnOn c1 c2) = doActionHelper (+) 1 mp c1 c2
    doAction mp (TurnOff c1 c2) = doActionHelper ((max 0 .) . subtract) 1 mp c1 c2
    doAction mp (Toggle c1 c2) = doActionHelper (+) 2 mp c1 c2

part2 = Map.foldl (+) 0 . doActions2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  actions <- return $ fromRight [] . traverse (parse actionParser "action parser") $ lines contents
  print $ part1 actions
  print $ part2 actions
