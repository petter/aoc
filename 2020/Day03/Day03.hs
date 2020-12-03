module Main where

import Data.Either (fromRight)
import qualified Data.Map.Lazy as M
import Data.List.Index (indexed)
import Data.Maybe (fromJust)

type Point = (Int, Int)
data GeologicalPoint 
    = Tree 
    | Open
    deriving (Show, Eq)
type GeologyMap = M.Map Point GeologicalPoint

parser :: [Char] -> GeologyMap
parser s = M.fromList $ foldl foldF [] indexedLines
    where
        foldF v l = v ++ lineParser l 
        indexedLines = indexed $ lines s
        lineParser (y, s) = fmap (\(x, c) -> ((x, y), cToGP c)) $ indexed s
        cToGP '.' = Open
        cToGP '#' = Tree
        cToGp _ = error "invalid char"

maxX :: GeologyMap -> Int
maxX = maximum . fmap fst . M.keys 

maxY :: GeologyMap -> Int
maxY = maximum . fmap snd . M.keys 

part1 :: GeologyMap -> Int
part1 gm = helper (0,0) 0
    where
        modValue = maxX gm + 1
        lastY = maxY gm + 1
        gpToInt gp = if gp == Tree then 1 else 0
        helper (x, y) trees = 
            if y == lastY then
                trees
            else
                helper (x + 3, y + 1) $ (trees +) $ gpToInt $ fromJust $ M.lookup (x `mod` modValue, y) gm
            

main :: IO ()
main = do
    file <- readFile "input.txt"
    let geologyMap = parser file
    print $ part1 geologyMap
