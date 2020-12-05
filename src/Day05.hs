-- | Solutions to day 5

module Day05 where

import           Data.List                      ( foldl'
                                                , sort
                                                )

-- Ranges indicate from the lower element inclusive to the higher element exclusive.
type Range = (Int, Int)

lowerHalf :: Range -> Range
lowerHalf (l, u) = (l, (l + u) `div` 2)

upperHalf :: Range -> Range
upperHalf (l, u) = ((l + u) `div` 2, u)

seatRow :: String -> Int
seatRow = fst . foldl' f (0, 128)
 where
  f range 'F' = lowerHalf range
  f range _   = upperHalf range

seatColumn :: String -> Int
seatColumn = fst . foldl' f (0, 8)
 where
  f range 'L' = lowerHalf range
  f range _   = upperHalf range

seatID :: String -> Int
seatID str = row * 8 + col
 where
  (rowStr, colStr) = splitAt 7 str
  row              = seatRow rowStr
  col              = seatColumn colStr

oneIDGap :: [Int] -> Int
oneIDGap []               = error "couldn't find gap"
oneIDGap [_             ] = error "couldn't find gap"
oneIDGap (a : as@(b : _)) = if a + 2 == b then a + 1 else oneIDGap as

day05 :: String -> String
day05 str =
  "Maximum seat ID: "
    ++ (show . last $ allSeatIDs)
    ++ "\n"
    ++ "My seat ID: "
    ++ (show . oneIDGap $ allSeatIDs)
    ++ "\n"
  where allSeatIDs = sort . map seatID $ lines str
