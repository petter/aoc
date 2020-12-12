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

data Action = MoveNorth Int
            | MoveSouth Int
            | MoveEast Int
            | MoveWest Int
            | TurnLeft Int
            | TurnRight Int
            | MoveForward Int
            deriving (Show)

type Ferry = (Int, (Int, Int)) -- (Deg, Pos)

parser :: String -> [Action]
parser = either (error . show) id . traverse (parse lineParser "line parser") . lines
    where
        lineParser = do
            actionString <- oneOf "NSEWLRF"
            num <- many1 digit

            let action = (case actionString of 'N' -> MoveNorth
                                               'S' -> MoveSouth
                                               'E' -> MoveEast
                                               'W' -> MoveWest
                                               'L' -> TurnLeft
                                               'R' -> TurnRight
                                               'F' -> MoveForward)
            pure $ action (read num)
                
manhattanDistance (x, y) = (abs x) + (abs y)

part1 :: [Action] -> Int
part1 actions = manhattanDistance $ snd $ move actions (0, (0,0))
    where
        forwardToAction 0 n = MoveEast n
        forwardToAction 90 n = MoveNorth n
        forwardToAction 180 n = MoveWest n
        forwardToAction 270 n = MoveSouth n
        forwardToAction deg n = error ("Invalid forwardToAction " ++ show deg ++ " " ++ show n)
        doAction (MoveNorth n) (deg, (x, y)) = (deg, (x, y + n))
        doAction (MoveSouth n) (deg, (x, y)) = (deg, (x, y - n))
        doAction (MoveEast n) (deg, (x, y)) = (deg, (x + n, y))
        doAction (MoveWest n) (deg, (x, y)) = (deg, (x - n, y))
        doAction (TurnLeft n) (deg, (x, y)) = ((deg + n) `mod` 360, (x, y))
        doAction (TurnRight n) (deg, (x, y)) = (newDeg `mod` 360, (x, y))
            where
                _deg = (deg - n)
                newDeg = 
                    if _deg < 0 then
                        360 + _deg
                    else 
                        _deg

        doAction (MoveForward n) (deg, (x, y)) = doAction (forwardToAction deg n) (deg, (x, y))
        move [] ferry = ferry
        move (a:as) ferry = move as $ doAction a ferry

main :: IO ()
main = do
    input <- parser <$> readFile "input.txt"
    print $ part1 input
