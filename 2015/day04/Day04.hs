module Day04 where

import qualified Data.Hash.MD5 as MD5
import Data.List (findIndex)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

part1 :: String -> Maybe Int
part1 input = findIndex correctHash hashes
  where
    correctHash :: String -> Bool
    correctHash = (=~ "^[0]{5}.*")
    hash = MD5.md5s . MD5.Str
    hashes = fmap (hash . (input ++) . show) [1 ..]

main :: IO ()
main = do
  input <- return "iwrupvqb"
  print $ part1 input