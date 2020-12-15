module Main where

import qualified Data.Map as M
import Data.List.Index (indexed)

part1 stopTurn start = rec (M.size startMap + 1) (last start) startMap
    where 
        startMap = M.fromList $ (\(turn, n) -> (n, turn + 1)) <$> indexed (init start)
        rec turn lastNum m = if turn == stopTurn then lastNum else rec (turn + 1) nextNum nextM
            where
                lastNumPrev = M.lookup lastNum m
                nextM = M.insert lastNum turn m
                nextNum = case lastNumPrev of (Just prevTurn) -> turn - prevTurn
                                              Nothing -> 0

main :: IO ()
main = do
    print $ part1 2020 [15,5,1,4,7,0]
    print $ part1 30000000 [15,5,1,4,7,0]
