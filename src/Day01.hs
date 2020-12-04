-- | Day 1 solution

module Day01
  ( day01
  ) where

import           Control.Monad                  ( guard )
import           Data.List                      ( tails )

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
