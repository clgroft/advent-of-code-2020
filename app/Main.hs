module Main where

import           Lib                            ( solutions )
import           Data.Array                     ( (!) )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  (hd : _) <- getArgs
  interact $ solutions ! read hd
