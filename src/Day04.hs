-- | Solution for day 4

module Day04
  ( day04
  ) where

import           Data.Char                      ( isDigit
                                                , isHexDigit
                                                )

splitIntoParagraphs :: String -> [String]
splitIntoParagraphs input = makeParagraphs $ dropWhile (== []) inputLines
 where
  inputLines = lines input
  makeParagraphs []  = []
  makeParagraphs lns = paragraph : makeParagraphs rest
   where
    (firstLines, remainder) = span (/= "") lns
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
