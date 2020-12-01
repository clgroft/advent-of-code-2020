module Lib
    ( day01
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
