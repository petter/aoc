module Main where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

containsTwoVowels :: String -> Bool
containsTwoVowels = (=~ (vowel ++ ".*" ++ vowel ++ ".*" ++ vowel))
  where
    vowel = "[aeiou]"

containsDoubleLetter :: String -> Bool
containsDoubleLetter = any (uncurry (==)) . (zip <*> tail)

notContainsUnwantedString :: String -> Bool
notContainsUnwantedString = not . (=~ "ab|cd|pq|xy")

isNiceString :: String -> Bool
isNiceString s = all id $ fmap ($ s) [containsTwoVowels, containsDoubleLetter, notContainsUnwantedString]

part1 :: String -> Int
part1 = length . filter isNiceString . lines

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents