{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Read (readMaybe)
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
import Text.Parsec.Char (space, newline, oneOf)
import Text.Parsec.String (Parser)
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (isJust)

data Passport = Passport 
        { byr :: Int
        , iyr :: Int
        , eyr :: Int
        , hgt :: String
        , hcl :: String
        , ecl :: String
        , pid :: String
        , cid :: Maybe String
        }
        deriving (Show)

fieldsToPassport :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Passport
fieldsToPassport (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) cid = Just $ Passport {byr = byr, iyr = iyr, eyr = eyr, hgt = hgt, hcl = hcl, ecl = ecl, pid = pid, cid = cid }
fieldsToPassport _ _ _ _ _ _ _ _ = Nothing

-- parser :: String -> [Passport]
parser :: String -> [Maybe Passport]
parser = fromRight (error "bad") . parse fileParser "passport parser"
    where 
        fileParser = passportParser `sepBy` newline
        passportParser = do 
            fields <- fieldParser `endBy` (try space <|> newline)

            let byr = fmap (read . snd) $ find (\(key, _) -> key == "byr") fields
            let iyr = fmap (read . snd) $ find (\(key, _) -> key == "iyr") fields
            let eyr = fmap (read . snd) $ find (\(key, _) -> key == "eyr") fields
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

part2 :: [Maybe Passport] -> Int
part2 = length . filter valid
    where 
        validYear year minYear maxYear = year >= minYear && year <= maxYear
        validHeight = fromRight False . parse validHeight' "height parser"
        validHeight' = do
            num :: Int <- fmap read $ many1 digit 
            unit <- many1 letter
            return (
                if unit == "cm" then
                    num >= 150 && num <= 193
                else 
                    num >= 59 && num <= 76
                )
        validHairColor = fromRight False . parse validHairColor' "hair color parser"
        validHairColor' = do
            char '#'
            count 6 $ oneOf "1234567890abcdef"
            return True
        validEyeColor ecl = any (ecl ==) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validPid pid = (length pid) == 9 && isJust (readMaybe pid :: Maybe Int)
            
        valid (Just (Passport byr iyr eyr hgt hcl ecl pid cid)) 
            =  validYear byr 1920 2002
            && validYear iyr 2010 2020
            && validYear eyr 2020 2030
            && validHeight hgt
            && validHairColor hcl
            && validEyeColor ecl
            && validPid pid
        valid _ = False

main :: IO ()
main = do
    file <- readFile "input.txt"
    let passports = parser file
    print $ part1 passports
    print $ part2 passports
