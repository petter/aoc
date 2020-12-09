module Main where

part1 :: [Int] -> Maybe Int
part1 nums = rec preamble startList
    where
        rec _ [] = Nothing
        rec prev25 (x:xs) = 
            if elem x (uncurry (+) <$> pairs) then 
                rec ((drop 1 prev25) ++ [x]) xs
            else 
                Just x
            where 
                pairs = [(a, b) | a <- prev25, b <- prev25, a < b]
                
        preamble = take 25 nums
        startList = drop 25 nums
    

main :: IO ()
main = do
    file <- readFile "input.txt"
    let nums = read <$> lines file
    print $ part1 nums
