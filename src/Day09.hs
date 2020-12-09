-- | Solutions to day 9

module Day09 where

import qualified Data.MultiSet                 as MS

preambleAndPairs :: [Integer] -> (MS.MultiSet Integer, [(Integer, Integer)])
preambleAndPairs ints = (MS.fromList start, zip ints rest)
  where (start, rest) = splitAt 25 ints

isSumOfTwo :: Integer -> MS.MultiSet Integer -> Bool
isSumOfTwo n ns = any hasOtherAddend . takeWhile ((< n) . (* 2)) $ MS.toList ns
  where hasOtherAddend k = (n - k) `MS.member` ns

firstNotSum :: MS.MultiSet Integer -> [(Integer, Integer)] -> Integer
firstNotSum _        []            = error "found no non-sum"
firstNotSum preamble ((a, b) : ps) = if b `isSumOfTwo` preamble
  then firstNotSum (b `MS.insert` (a `MS.delete` preamble)) ps
  else b

findContiguousAddends :: Integer -> [Integer] -> [Integer]
findContiguousAddends n ns = f (tail offOne) (zipWith twoList offOne ns)
 where
  offOne = tail ns
  twoList a b = [a, b]
  f remainders lists = case filter ((== n) . sum) lists of
    []      -> f (tail remainders) (zipWith (:) remainders lists)
    (l : _) -> l

findContiguousSum :: Integer -> [Integer] -> Integer
findContiguousSum n ns = minimum addends + maximum addends
  where addends = findContiguousAddends n ns

day09 :: String -> String
day09 input =
  "First not sum of preceding: "
    ++ show firstAnswer
    ++ "\n"
    ++ "Encryption weakness: "
    ++ show contiguousSum
    ++ "\n"
 where
  listNumbers       = map read $ lines input :: [Integer]
  (preamble, pairs) = preambleAndPairs listNumbers
  firstAnswer       = firstNotSum preamble pairs
  contiguousSum     = findContiguousSum firstAnswer listNumbers
