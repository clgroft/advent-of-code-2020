-- | Solution for day 11

module Day11
  ( day11
  ) where

import qualified Data.Map.Strict               as M

data SeatState = Empty | Full deriving Eq
type Position = (Int, Int)
data SeatingArea = SeatingArea
  { numRows :: Int
  , numCols :: Int
  , seats   :: M.Map Position SeatState
  }
  deriving Eq

initialArea :: String -> SeatingArea
initialArea input = SeatingArea nRows nCols sts
 where
  lns   = lines input
  nRows = length lns
  nCols = length (head lns)
  sts   = initialArea' input

initialArea' :: String -> M.Map Position SeatState
initialArea' = M.fromList . concat . zipWith initialRow [0 ..] . lines
 where
  initialRow :: Int -> String -> [(Position, SeatState)]
  initialRow rowIdx = map addRowIdx . filter ((== 'L') . snd) . zip [0 ..]
    where addRowIdx (colIdx, _) = ((rowIdx, colIdx), Empty)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

neighbors :: Position -> [Position]
neighbors (x, y) =
  [ (x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0 ]

numberFilledSeats :: SeatingArea -> Int
numberFilledSeats = count (== Full) . M.elems . seats

evolveSeat :: SeatingArea -> Position -> SeatState -> SeatState
evolveSeat sa p ss = if neighborCount == 0
  then Full
  else if neighborCount >= 4 then Empty else ss
 where
  neighborCount = count isFull $ neighbors p
    where isFull p' = seats sa M.!? p' == Just Full

evolve :: SeatingArea -> SeatingArea
evolve sa = SeatingArea (numRows sa)
                        (numCols sa)
                        (M.mapWithKey (evolveSeat sa) $ seats sa)

neighbors' :: SeatingArea -> Position -> [Position]
neighbors' (SeatingArea nRows nCols sts) (r, c) = concat
  [n, nw, w, sw, s, se, e, ne]
 where
  firstSeat ps = case (filter (`M.member` sts) ps) of
    []      -> []
    (p : _) -> [p]
  n   = firstSeat $ takeWhile ((>= 0) . fst) [ (r - d, c) | d <- [1 ..] ]
  nws = takeWhile (\p -> fst p >= 0 && snd p >= 0)
                  [ (r - d, c - d) | d <- [1 ..] ]
  nw  = firstSeat nws
  w   = firstSeat $ takeWhile ((>= 0) . snd) [ (r, c - d) | d <- [1 ..] ]
  sws = takeWhile (\p -> fst p < nRows && snd p >= 0)
                  [ (r + d, c - d) | d <- [1 ..] ]
  sw  = firstSeat sws
  s   = firstSeat $ takeWhile ((< nRows) . fst) [ (r + d, c) | d <- [1 ..] ]
  ses = takeWhile (\p -> fst p < nRows && snd p < nCols)
                  [ (r + d, c + d) | d <- [1 ..] ]
  se  = firstSeat ses
  e   = firstSeat $ takeWhile ((< nCols) . snd) [ (r, c + d) | d <- [1 ..] ]
  nes = takeWhile (\p -> fst p >= 0 && snd p < nCols)
                  [ (r - d, c + d) | d <- [1 ..] ]
  ne = firstSeat nes

evolveSeat' :: SeatingArea -> Position -> SeatState -> SeatState
evolveSeat' sa p ss = if neighborCount == 0
  then Full
  else if neighborCount >= 5 then Empty else ss
 where
  neighborCount = count isFull $ neighbors' sa p
    where isFull p' = seats sa M.!? p' == Just Full

evolve' :: SeatingArea -> SeatingArea
evolve' sa = SeatingArea (numRows sa)
                         (numCols sa)
                         (M.mapWithKey (evolveSeat' sa) $ seats sa)

firstFixedPoint :: (Eq a) => (a -> a) -> a -> a
firstFixedPoint f start = ff $ iterate f start
 where
  ff :: (Eq a) => [a] -> a
  ff []               = error "impossible"
  ff [_             ] = error "impossible"
  ff (a : as@(b : _)) = if a == b then a else ff as

day11 :: String -> String
day11 input =
  "Number of stable filled seats (old rules): "
    ++ show numFilled
    ++ "\n"
    ++ "Number of stable filled seats (new rules): "
    ++ show numFilled'
    ++ "\n"
 where
  initArea   = initialArea input
  numFilled  = numberFilledSeats . firstFixedPoint evolve $ initArea
  numFilled' = numberFilledSeats . firstFixedPoint evolve' $ initArea
