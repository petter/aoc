module Main where

import qualified Data.Hash.MD5 as MD5
import Data.List (findIndex)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

hashMatching :: String -> Maybe Int
hashMatching regex = fmap (+ 1) $ findIndex correctHash hashes
  where
    correctHash :: String -> Bool
    correctHash = (=~ regex)
    hash = MD5.md5s . MD5.Str
    hashes = fmap (hash . (input ++) . show) [1 ..]
    input = "iwrupvqb"

part1 :: Maybe Int
part1 = hashMatching "^[0]{5}.*"

part2 :: Maybe Int
part2 = hashMatching "^[0]{6}.*"

main :: IO ()
main = do
  print $ part1
  print $ part2
