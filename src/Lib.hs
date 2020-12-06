module Lib
  ( solutions
  ) where

import           Day01                          ( day01 )
import           Day02                          ( day02 )
import           Day03                          ( day03 )
import           Day04                          ( day04 )
import           Day05                          ( day05 )
import           Day06                          ( day06 )
import           Data.Array                     ( Array
                                                , listArray
                                                )
-- import           Debug.Trace                    ( trace )

solutions :: Array Int (String -> String)
solutions = listArray (1, 25) [day01, day02, day03, day04, day05, day06]
