-- | Day 2 solution

module Day02
  ( day02
  ) where

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
  entries      = words $ subSpaces line
  minCountWord = head entries
  maxCountWord = entries !! 1
  letterWord   = entries !! 2
  passwd       = entries !! 3

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
