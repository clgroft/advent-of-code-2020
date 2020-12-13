-- | Solutions to day 13
-- | Note: the solution to part 2 is wrong somehow.
-- | It works on all the test cases though, so I don't know what the issue is

module Day13
  ( day13
  ) where

import           Data.Char                      ( isDigit )
import           Data.List                      ( minimumBy )
import           Data.List.Split                ( splitOn )

day13 :: String -> String
day13 input = output
 where
  [line1, line2]    = lines input
  numMinutesWaiting = read line1 :: Int
  buses             = splitOn "," line2
  activeBuses       = map read $ filter (all isDigit) buses :: [Int]
  firstBusAndTime   = minimumBy (\a b -> compare (snd a) (snd b)) $ map
    (\n -> (n, head . dropWhile (< numMinutesWaiting) $ map (n *) [0 ..]))
    activeBuses
  answer1 = a * (b - numMinutesWaiting) where (a, b) = firstBusAndTime
  busesAndIndices = zip [0, -1 ..] buses
  activeBusesAndIndices = map (\(a, b) -> (a, read b :: Int))
    $ filter (all isDigit . snd) busesAndIndices
  correctedBusesAndIndices =
    map (\(a, b) -> (a `mod` b, b)) activeBusesAndIndices
  answer2 = crt correctedBusesAndIndices
  output  = show answer1 ++ "\n" ++ show answer2 ++ "\n"

-- extGCD n m = (k, a, b) where gcd(m,n) = an + bm = k.  Assume n, m >= 0.
extGCD :: Int -> Int -> (Int, Int, Int)
extGCD n m | n < m = (k, b, a) where (k, a, b) = extGCD m n
extGCD n 0         = (n, 1, 0)
extGCD n m         = (k, a, b) where
  (q, r)      = n `divMod` m
  (k, a', b') = extGCD m r
  (a, b)      = (b', a' - q * b')

-- crt2 (a, m) (b, n) = (c, m * n) where a = c mod m and b = c mod n.
-- Assumes m, n coprime.
crt2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
crt2 (a, m) (b, n) = (c, m * n)
 where
  (_, p, _) = extGCD m n -- pm + qn = 1
  c         = (a + m * (b - a) * p) `mod` (m * n)

crt :: [(Int, Int)] -> Int
crt ps = c where (c, _) = foldl crt2 (0, 1) ps
