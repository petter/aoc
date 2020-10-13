module Day01 where

parseFloor :: Char -> Int -> Int
parseFloor '(' n = n + 1
parseFloor ')' n = n - 1

floorParserHelper :: [Char] -> Int -> Int
floorParserHelper (c : cs) n = floorParserHelper cs $ parseFloor c n
floorParserHelper [] n = n

floorParser :: [Char] -> Int
floorParser s = floorParserHelper s 0

floorParser2Helper :: [Char] -> Int -> Int -> Int
floorParser2Helper _ (-1) charPos = charPos
floorParser2Helper [] _ _ = -1
floorParser2Helper (c : cs) n charPos = floorParser2Helper cs (parseFloor c n) (charPos + 1)

floorParser2 :: [Char] -> Int
floorParser2 s = floorParser2Helper s 0 0

main :: IO ()
main = do
  contents <- readFile "day01.txt"
  print $ floorParser contents
  print $ floorParser2 contents