module Day01 where

floorParserHelper :: [Char] -> Int -> Int
floorParserHelper ('(' : cs) n = floorParserHelper cs (n + 1)
floorParserHelper (')' : cs) n = floorParserHelper cs (n - 1)
floorParserHelper [] n = n

floorParser :: [Char] -> Int
floorParser s = floorParserHelper s 0

main :: IO ()
main = do
  contents <- readFile "day01.txt"
  putStrLn $ show $ floorParser contents