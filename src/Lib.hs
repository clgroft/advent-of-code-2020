module Lib
    ( someFunc
    , day01
    , day01'
    ) where

import Data.List (tails)
import Control.Monad (guard)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

findExpenses :: [Int] -> Int
findExpenses expenses = head $ [ x * y | (x:xs) <- tails expenses, y <- xs, x + y == 2020 ]

day01 :: IO ()
day01 = interact f
  where
    f = (++"\n") . show . findExpenses . map read . lines

findExpenses' :: [Int] -> Int
findExpenses' expenses = head $ do
  x:xs <- tails expenses
  y:ys <- tails xs
  z    <- ys
  guard  $ x + y + z == 2020
  return $ x * y * z

day01' :: IO ()
day01' = interact f
  where
    f = (++"\n") . show . findExpenses' . map read . lines
