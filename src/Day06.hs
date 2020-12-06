-- | Solutions to day 6

module Day06
  ( day06
  ) where

import           Data.Set                       ( fromList
                                                , size
                                                , intersection
                                                )
import           Data.List                      ( foldl1' )

splitIntoParagraphs :: String -> [[String]]
splitIntoParagraphs input = makeParagraphs $ dropWhile (== "") inputLines
 where
  inputLines = lines input
  makeParagraphs []  = []
  makeParagraphs lns = paragraph : makeParagraphs rest
   where
    (paragraph, remainder) = span (/= "") lns
    rest                   = dropWhile (== "") remainder

numYeses :: [String] -> Int
numYeses strs = size . fromList $ concat strs

numYesesAll :: [String] -> Int
numYesesAll strs = size . foldl1' intersection $ map fromList strs

day06 :: String -> String
day06 input =
  "Total yeses: "
    ++ show (sum . map numYeses $ paragraphs)
    ++ "\n"
    ++ "Total all-yeses: "
    ++ show (sum . map numYesesAll $ paragraphs)
    ++ "\n"
  where paragraphs = splitIntoParagraphs input
