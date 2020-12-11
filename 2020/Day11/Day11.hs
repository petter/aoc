module Main where

import Data.List.Index
import Data.Maybe
import qualified Data.Map as M

type Point = (Int, Int)
data Seat = EmptySeat
          | OccupiedSeat
          deriving (Show, Eq)
type SeatMap = M.Map Point Seat

isEmpty :: Seat -> Bool
isEmpty EmptySeat = True
isEmpty OccupiedSeat = False

parseFile :: String -> SeatMap
parseFile f = M.fromList (foldMap indexedLineParser indexedLines)
    where
        indexedLines = indexed $ lines f
        indexedLineParser (lineNum, cs) = (\(colNum, seat) -> ((colNum, lineNum), seat)) <$> csToIndexedSeat cs
        csToIndexedSeat = foldl (\seats indexedSeat -> seats ++ removeNothing indexedSeat) [] . indexed . fmap charToSeat
        removeNothing (i, (Just a)) = [(i, a)]
        removeNothing (i, Nothing) = []
        charToSeat 'L' = Just EmptySeat
        charToSeat '#' = Just OccupiedSeat
        charToSeat '.' = Nothing

adjacentSeats :: SeatMap -> Point -> [Seat]
adjacentSeats seatMap (x,y) = catMaybes ((flip M.lookup) seatMap <$> adjacentPoints)
    where
        adjacentPoints = [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]


part1 :: SeatMap -> Int
part1 seatMap = M.size $ M.filter (== OccupiedSeat) $ rec seatMap
    where
        rec updatedMap = 
            if nextMap == updatedMap then
                nextMap 
            else 
                rec nextMap 
            where
                nextMap = M.foldrWithKey (\point seat m -> M.insert point (nextSeat point seat updatedMap) m) M.empty updatedMap
        nextSeat point EmptySeat seatMap = if all isEmpty $ adjacentSeats seatMap point then OccupiedSeat else EmptySeat
        nextSeat point OccupiedSeat seatMap = if (length $ filter (==OccupiedSeat) (adjacentSeats seatMap point)) >= 4 then EmptySeat else OccupiedSeat

main :: IO ()
main = do
    file <- readFile "input.txt"
    let seatMap = parseFile file
    print $ part1 seatMap 
