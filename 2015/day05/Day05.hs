module Main where

import Data.List (isInfixOf)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Control.Monad

containsTwoVowels :: String -> Bool
containsTwoVowels = (=~ (vowel ++ ".*" ++ vowel ++ ".*" ++ vowel))
  where
    vowel = "[aeiou]"

containsDoubleLetter :: String -> Bool
containsDoubleLetter = any (uncurry (==)) . (zip <*> tail)

notContainsUnwantedString :: String -> Bool
notContainsUnwantedString = not . (=~ "ab|cd|pq|xy")

isNiceString :: [String -> Bool] -> String -> Bool
isNiceString rules s = all id $ fmap ($ s) rules

-- (.&&.) = liftM2 (&&)

part1 :: String -> Int
-- part1 = length . filter (containsTwoVowels .&&. notContainsUnwantedString .&&. containsDoubleLetter) . lines

part1 = length . filter (liftM3 (((&&) .) . (&&)) containsTwoVowels notContainsUnwantedString containsDoubleLetter) . lines


containsReoccuringPair :: String -> Bool
containsReoccuringPair (c1 : c2 : cs)
  | [c1, c2] `isInfixOf` cs = True
  | otherwise = containsReoccuringPair (c2 : cs)
containsReoccuringPair _ = False

containsReoccuringLetter :: String -> Bool
containsReoccuringLetter (c1 : c2 : c3 : cs)
  | c1 == c3 = True
  | otherwise = containsReoccuringLetter (c2 : c3 : cs)
containsReoccuringLetter _ = False

part2 :: String -> Int
part2 = length . filter (isNiceString [containsReoccuringPair, containsReoccuringLetter]) . lines

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
