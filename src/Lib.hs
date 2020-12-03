module Lib
  ( day01
  , day02
  , day03
  ) where

import           Data.List                      ( tails )
import           Control.Monad                  ( guard )
import           Data.Array                     ( Array
                                                , listArray
                                                , (!)
                                                , bounds
                                                , indices
                                                , assocs
                                                , elems
                                                )
import           Debug.Trace                    ( trace )

-- Day 1
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

-- Day 2
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
  [minCountWord, maxCountWord, letterWord, passwd] = words $ subSpaces line

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

-- Day 3
type TobogganField = Array (Int, Int) Char
-- type TobogganField = [String]

isTree :: Char -> Bool
isTree c = trace [c] ('#' == c)

getFromField :: TobogganField -> (Int, Int) -> Char
getFromField field idx = trace (show idx) (field ! idx)

numTrees :: Int -> Int -> TobogganField -> Int
numTrees down right field =
  length . filter isTree . map (getFromField field) $ idxs
-- numTrees down right field =
--   length . filter (== '#') . map (\(d, r) -> (field !! d) !! r) $ trace
--     (show idxs)
--     idxs
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
  "Field bounds: "
    ++ show (bounds field)
  --   ++ "\nEntire field: "
  --   ++ (show $ elems field)
    -- ++ (show $ assocs field)
    ++ "\nNumber of trees: "
    ++ show trees
    ++ "\nProduct over all slopes: "
    ++ show treesProduct
    ++ "\n"
 where
  -- field = fieldFromLines . map init $ lines input
  field        = fieldFromLines $ lines input
  trees        = numTrees 1 3 field
  trees2       = numTrees 1 1 field
  trees3       = numTrees 1 5 field
  trees4       = numTrees 1 7 field
  trees5       = numTrees 2 1 field
  treesProduct = trees * trees2 * trees3 * trees4 * trees5
  -- (_, (ht, wd)) = bounds field
  -- height        = ht + 1
  -- width         = wd + 1
  -- nextCell (d, r) = (d + 1, (r + 3) `mod` width)
