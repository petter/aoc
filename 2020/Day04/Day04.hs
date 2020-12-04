module Main where

import Data.Either (fromRight)
import Text.Parsec
  ( alphaNum,
    letter,
    char,
    choice,
    count,
    digit,
    many1,
    skipMany1,
    parse,
    string,
    try,
    sepBy,
    endBy,
    ParseError,
    (<|>)
  )
import Text.Parsec.Char (space, newline)
import Text.Parsec.String (Parser)
import Data.List (find)
import Data.Maybe (isJust)

data Passport = Passport 
        { byr :: String
        , iyr :: String
        , eyr :: String
        , hgt :: String
        , hcl :: String
        , ecl :: String
        , pid :: String
        , cid :: Maybe String
        }
        deriving (Show)

fieldsToPassport :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Passport
fieldsToPassport (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) cid = Just $ Passport {byr = byr, iyr = iyr, eyr = eyr, hgt = hgt, hcl = hcl, ecl = ecl, pid = pid, cid = cid }
fieldsToPassport _ _ _ _ _ _ _ _ = Nothing

-- parser :: String -> [Passport]
parser :: String -> [Maybe Passport]
parser = fromRight (error "bad") . parse fileParser "passport parser"
    where 
        fileParser = passportParser `sepBy` newline
        passportParser = do 
            fields <- fieldParser `endBy` (try space <|> newline)

            let byr = fmap snd $ find (\(key, _) -> key == "byr") fields
            let iyr = fmap snd $ find (\(key, _) -> key == "iyr") fields
            let eyr = fmap snd $ find (\(key, _) -> key == "eyr") fields
            let hgt = fmap snd $ find (\(key, _) -> key == "hgt") fields
            let hcl = fmap snd $ find (\(key, _) -> key == "hcl") fields
            let ecl = fmap snd $ find (\(key, _) -> key == "ecl") fields
            let pid = fmap snd $ find (\(key, _) -> key == "pid") fields
            let cid = fmap snd $ find (\(key, _) -> key == "cid") fields

            return $ fieldsToPassport byr iyr eyr hgt hcl ecl pid cid

        fieldParser = do
            key <- count 3 letter
            char ':'
            value <- many1 $ try alphaNum <|> char '#'
            return (key, value)

part1 :: [Maybe Passport] -> Int
part1 = length . filter isJust

main :: IO ()
main = do
    file <- readFile "input.txt"
    let passports = parser file
    print $ part1 passports
