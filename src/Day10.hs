-- | Solutions to day 10

module Day10
  ( day10
  ) where

import           Data.List                      ( sort )

day10 :: String -> String
day10 input =
  "Product of 1-diff and 3-diff: "
    ++ (show $ productOneThree adapters)
    ++ "\n"
    ++ "Number of arrangements: "
    ++ (show . snd . head $ allArrangements adapters)
    ++ "\n"
  where adapters = allAdapters input

type Adapter = Int

allAdapters :: String -> [Adapter]
allAdapters = (0 :) . sort . map read . lines

productOneThree :: [Adapter] -> Int
productOneThree adapters =
  count (== 1) differences * (1 + count (== 3) differences)
 where
  differences = zipWith (-) (tail adapters) adapters
  count p = length . filter p

allArrangements :: [Adapter] -> [(Adapter, Int)]
allArrangements adapters = foldr f [(last adapters + 3, 1)] adapters
  where f a ps = (a, sum . map snd $ takeWhile ((<= a + 3) . fst) ps) : ps
