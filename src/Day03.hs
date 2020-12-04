-- | Solution for day 3

module Day03
  ( day03
  ) where

import           Data.Array                     ( Array
                                                , listArray
                                                , (!)
                                                , bounds
                                                )

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
  field        = fieldFromLines $ lines input
  trees        = numTrees 1 3 field
  trees2       = numTrees 1 1 field
  trees3       = numTrees 1 5 field
  trees4       = numTrees 1 7 field
  trees5       = numTrees 2 1 field
  treesProduct = trees * trees2 * trees3 * trees4 * trees5
