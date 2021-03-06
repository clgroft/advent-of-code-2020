module Lib
  ( solutions
  ) where

import           Day01                          ( day01 )
import           Day02                          ( day02 )
import           Day03                          ( day03 )
import           Day04                          ( day04 )
import           Day05                          ( day05 )
import           Day06                          ( day06 )
import           Day07                          ( day07 )
import           Day08                          ( day08 )
import           Day09                          ( day09 )
import           Day10                          ( day10 )
import           Day11                          ( day11 )
import           Day12                          ( day12 )
import           Day13                          ( day13 )
import           Data.Array                     ( Array
                                                , listArray
                                                )

solutions :: Array Int (String -> String)
solutions = listArray
  (1, 25)
  [ day01
  , day02
  , day03
  , day04
  , day05
  , day06
  , day07
  , day08
  , day09
  , day10
  , day11
  , day12
  , day13
  ]
