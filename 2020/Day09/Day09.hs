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
    
part2 :: Int -> [Int] -> Maybe Int
part2 _ [] = Nothing
part2 n xs = 
    if sum nums == n then
        Just (minimum nums + maximum nums)
    else 
        part2 n $ tail xs
    where
        nums = takeWhileList ((< n) . sum) xs

takeWhileList shouldTake xs = rec [] xs
    where
        rec curList (y:ys) = if shouldTake curList then rec (curList ++ [y]) ys else curList
main :: IO ()
main = do
    file <- readFile "input.txt"
    let nums = read <$> lines file
    let invalidNum = part1 nums
    print invalidNum
    print $ (flip part2) nums <$> invalidNum
