module Lib
  ( day01
  , day02
  , day03
  , day04
  ) where

import           Data.Char                      ( isDigit
                                                , isHexDigit
                                                )
import           Data.List                      ( tails )
import           Control.Monad                  ( guard )
import           Data.Array                     ( Array
                                                , listArray
                                                , (!)
                                                , bounds
                                                )
-- import           Debug.Trace                    ( trace )

-- Day 1
findTwo :: [Integer] -> Integer
findTwo expenses =
  head $ [ x * y | (x : xs) <- tails expenses, y <- xs, x + y == 2020 ]

findThree :: [Integer] -> Integer
findThree expenses = head $ do
  x : xs <- tails expenses
  y : ys <- tails xs
  z      <- ys
  guard $ x + y + z == 2020
  return $ x * y * z

day01 :: String -> String
day01 input =
  "Product of two:   "
    ++ show productOfTwo
    ++ "\n"
    ++ "Product of three: "
    ++ show productOfThree
    ++ "\n"
 where
  expenses       = map read $ lines input :: [Integer]
  productOfTwo   = findTwo expenses
  productOfThree = findThree expenses

-- Day 2
data Password = Password
  { minCount :: Int
  , maxCount :: Int
  , letter   :: Char
  , password :: String
  }

subSpaces :: String -> String
subSpaces = map f
 where
  f '-' = ' '
  f ':' = ' '
  f x   = x

parsePassword :: String -> Password
parsePassword line = Password (read minCountWord)
                              (read maxCountWord)
                              (head letterWord)
                              passwd
 where
  [minCountWord, maxCountWord, letterWord, passwd] = words $ subSpaces line

isLegal :: Password -> Bool
isLegal pass = minCount pass <= letterCount && letterCount <= maxCount pass
  where letterCount = length . filter (== letter pass) $ password pass

isLegal' :: Password -> Bool
isLegal' (Password minCount maxCount letter password) =
  ((password !! (minCount - 1)) == letter)
    /= ((password !! (maxCount - 1)) == letter)

day02 :: String -> String
day02 input =
  "Legal passwords:     "
    ++ show legalPasswordCount
    ++ "\n"
    ++ "New legal passwords: "
    ++ show legalPasswordCount'
    ++ "\n"
 where
  passwords           = map parsePassword $ lines input
  legalPasswordCount  = length . filter isLegal $ passwords
  legalPasswordCount' = length . filter isLegal' $ passwords

-- Day 3
type TobogganField = Array (Int, Int) Char

numTrees :: Int -> Int -> TobogganField -> Int
numTrees down right field = length . filter ('#' ==) . map (field !) $ idxs
 where
  (_, (ht, wd)) = bounds field
  height        = ht + 1
  width         = wd + 1
  nextCell (d, r) = (d + down, (r + right) `mod` width)
  idxs = takeWhile (\(d, _) -> d < height) . tail $ iterate nextCell (0, 0)

fieldFromLines :: [String] -> TobogganField
fieldFromLines lns = listArray ((0, 0), (ht, wd)) $ concat lns
 where
  ht = length lns - 1
  wd = length (head lns) - 1

day03 :: String -> String
day03 input =
  "Number of trees: "
    ++ show trees
    ++ "\nProduct over all slopes: "
    ++ show treesProduct
    ++ "\n"
 where
  -- field = fieldFromLines . map init $ lines input
  field        = fieldFromLines $ lines input
  trees        = numTrees 1 3 field
  trees2       = numTrees 1 1 field
  trees3       = numTrees 1 5 field
  trees4       = numTrees 1 7 field
  trees5       = numTrees 2 1 field
  treesProduct = trees * trees2 * trees3 * trees4 * trees5

-- Day 4
splitIntoParagraphs :: String -> [String]
splitIntoParagraphs input = makeParagraphs $ dropWhile (== []) inputLines
 where
  inputLines = lines input
  makeParagraphs []    = []
  makeParagraphs lines = paragraph : makeParagraphs rest
   where
    (firstLines, remainder) = span (/= "") lines
    paragraph               = unwords firstLines
    rest                    = dropWhile (== "") remainder

birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportID
  :: String
birthYear = "byr"
issueYear = "iyr"
expirationYear = "eyr"
height = "hgt"
hairColor = "hcl"
eyeColor = "ecl"
passportID = "pid"
-- countryID = "cid"

credentialFieldNames :: [String]
credentialFieldNames =
  [ birthYear
  , issueYear
  , expirationYear
  , height
  , hairColor
  , eyeColor
  , passportID
  ]

data PassportField = PassportField
  { fieldName  :: String
  , fieldValue :: String
  }
  deriving Show

parsePassportField :: String -> PassportField
parsePassportField str = PassportField name value
 where
  (name, rest) = span (/= ':') str
  value        = tail rest

type Passport = [PassportField]

parsePassport :: String -> Passport
parsePassport str = map parsePassportField $ words str

isFirstPassValidCredentials :: Passport -> Bool
isFirstPassValidCredentials passport = all
  (\name -> any (\field -> fieldName field == name) passport)
  credentialFieldNames

isNumberInRange :: Int -> Int -> String -> Bool
isNumberInRange minNum maxNum str =
  all isDigit str && minNum <= val && val <= maxNum
  where val = read str :: Int

isValidBirthYear :: String -> Bool
isValidBirthYear = isNumberInRange 1920 2002

isValidIssueYear :: String -> Bool
isValidIssueYear = isNumberInRange 2010 2020

isValidExpirationYear :: String -> Bool
isValidExpirationYear = isNumberInRange 2020 2030

isValidHeight :: String -> Bool
isValidHeight str = case unit of
  "cm" -> isNumberInRange 150 193 cnt
  "in" -> isNumberInRange 59 76 cnt
  _    -> False
  where (cnt, unit) = span isDigit str

isValidHairColor :: String -> Bool
isValidHairColor str =
  length str == 7 && head str == '#' && all isHexDigit (tail str)

isValidEyeColor :: String -> Bool
isValidEyeColor = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

isValidPassportID :: String -> Bool
isValidPassportID str = length str == 9 && all isDigit str

isValidField :: PassportField -> Bool
isValidField (PassportField name value) = case name of
  "byr" -> isValidBirthYear value
  "iyr" -> isValidIssueYear value
  "eyr" -> isValidExpirationYear value
  "hgt" -> isValidHeight value
  "hcl" -> isValidHairColor value
  "ecl" -> isValidEyeColor value
  "pid" -> isValidPassportID value
  "cid" -> True
  _     -> False

day04 :: String -> String
day04 input =
  "Number of valid credentials: "
    ++ show numValid
    ++ "\n"
    ++ "Number of truly valid credentials: "
    ++ show numTrulyValid
    ++ "\n"
 where
  passports                 = map parsePassport $ splitIntoParagraphs input
  firstPassValidCredentials = filter isFirstPassValidCredentials passports
  numValid                  = length firstPassValidCredentials
  numTrulyValid =
    length . filter (all isValidField) $ firstPassValidCredentials
