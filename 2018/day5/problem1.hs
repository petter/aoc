import Data.List
import Data.Char
import System.IO

polymerReact :: [Char] -> [Char]
polymerReact x
    | newStr /= x = polymerReact newStr
    | otherwise = newStr
    where 
        newStr = polymerReactCycle x

polymerReactCycle :: [Char] -> [Char]
polymerReactCycle [] = []
polymerReactCycle (a:[]) = [a]
polymerReactCycle (a:b:abs) 
    | a /= b && (toLower a) == (toLower b) = polymerReactCycle abs
    | otherwise = a : (polymerReactCycle (b:abs))


main = do
    contents <- readFile "input.txt"
    putStrLn (show (length (polymerReact contents)))

