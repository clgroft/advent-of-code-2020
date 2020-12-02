module Lib
    ( day01
    , day02
    ) where

import Data.List (tails)
import Control.Monad (guard)

-- Day 1
findTwo :: [Integer] -> Integer
findTwo expenses = head $ [ x * y | (x:xs) <- tails expenses, y <- xs, x + y == 2020 ]

findThree :: [Integer] -> Integer
findThree expenses = head $ do
  x:xs <- tails expenses
  y:ys <- tails xs
  z    <- ys
  guard  $ x + y + z == 2020
  return $ x * y * z

day01 :: IO ()
day01 = interact f
  where
    f input = "Product of two:   " ++ show productOfTwo ++ "\n"
           ++ "Product of three: " ++ show productOfThree ++ "\n"
      where
        expenses = map read $ lines input :: [Integer]
        productOfTwo = findTwo expenses
        productOfThree = findThree expenses

-- Day 2
data Password = Password { minCount :: Int
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
parsePassword line = Password (read minCountWord) (read maxCountWord) (head letterWord) passwd
  where
    [minCountWord, maxCountWord, letterWord, passwd] = words $ subSpaces line

isLegal :: Password -> Bool
isLegal pass = minCount pass <= letterCount && letterCount <= maxCount pass
  where
    letterCount = length . filter (== letter pass) $ password pass

isLegal' :: Password -> Bool
isLegal' (Password minCount maxCount letter password)
  = ((password !! (minCount - 1)) == letter) /= ((password !! (maxCount - 1)) == letter)

day02 :: IO ()
day02 = interact f
  where
    f input = "Legal passwords:     " ++ show legalPasswordCount  ++ "\n"
           ++ "New legal passwords: " ++ show legalPasswordCount' ++ "\n"
      where
        passwords = map parsePassword $ lines input
        legalPasswordCount  = length . filter isLegal  $ passwords
        legalPasswordCount' = length . filter isLegal' $ passwords
