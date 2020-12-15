module Main where

import qualified Data.Map as M
import Data.List.Index (indexed)

part1 start = rec (M.size startMap + 1) (last start) startMap
    where 
        startMap = M.fromList $ (\(turn, n) -> (n, turn + 1)) <$> indexed (init start)
        rec 2020 lastNum _ = lastNum
        rec turn lastNum m = rec (turn + 1) nextNum nextM
            where
                lastNumPrev = M.lookup lastNum m
                nextM = M.insert lastNum turn m
                nextNum = case lastNumPrev of (Just prevTurn) -> turn - prevTurn
                                              Nothing -> 0

main :: IO ()
main = do
    print $ part1 [15,5,1,4,7,0]
