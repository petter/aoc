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
                newDeg' = deg - n
                newDeg = 
                    if newDeg'< 0 then
                        360 + newDeg'
                    else
                        newDeg'

        doAction (MoveForward n) (deg, (x, y)) = doAction (forwardToAction deg n) (deg, (x, y))
        move [] ferry = ferry
        move (a:as) ferry = move as $ doAction a ferry

part2 :: [Action] -> Int
part2 actions = manhattanDistance $ snd $ move actions ((10,1), (0,0))
    where
        doAction (MoveNorth n) ((x, y), pos) = ((x, y + n), pos)
        doAction (MoveSouth n) ((x, y), pos) = ((x, y - n), pos)
        doAction (MoveEast n) ((x, y), pos) = ((x + n, y), pos)
        doAction (MoveWest n) ((x, y), pos) = ((x - n, y), pos)
        doAction (TurnLeft n) (waypoint, pos) = (rotatePoint waypoint n, pos)
        doAction (TurnRight n) (waypoint, pos) = (rotatePoint waypoint (negate n), pos)
        doAction (MoveForward n) ((wx, wy), (x, y)) = ((wx, wy), (x + wx * n, y + wy * n))
        move [] ferry = ferry
        move (a:as) ferry = move as $ doAction a ferry


rotatePoint :: (Int, Int) -> Int -> (Int, Int)
rotatePoint (x, y) deg = (round x', round y')
    where 
        deg' = fromIntegral $ 
            if deg < 0 then
                360 + deg
            else
                deg
        rad = deg' * (pi / 180)
        x'' = fromIntegral x
        y'' = fromIntegral y
        x' = x'' * cos rad - y'' * sin rad
        y' = y'' * cos rad + x'' * sin rad

main :: IO ()
main = do
    input <- parser <$> readFile "input.txt"
    print $ part1 input
    print $ part2 input
