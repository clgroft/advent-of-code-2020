-- | Solutions for day 7

module Day07
  ( day07
  ) where

import           Data.List                      ( foldl' )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as M
import qualified Data.Set                      as S

type BagColor = String

-- outer directly contains numContained copies of inner
data Contain = Contain
  { outer        :: BagColor
  , inner        :: BagColor
  , numContained :: Int
  }

-- Turn a word-split line into a list of Contains.
-- Note that bag colors which contain no other bags will return nothing.
listContainsWords :: [String] -> [Contain]
listContainsWords wds = case length wds of
  7 -> []
  _ -> map f . chunksOf 4 $ drop 4 wds
 where
  outerBag = unwords $ take 2 wds
  f [n, p, c, _] = Contain outerBag (unwords [p, c]) (read n)
  f _            = error "parse error"

-- Turn a line-split input into a list of all Contains.
-- Note that bag colors which contain no other bags will not appear.
listContainsLines :: [String] -> [Contain]
listContainsLines = concatMap (listContainsWords . words)

-- Tracks which bag colors directly contain given bag colors.
-- Bags which are not contained by any other bags will not appear.
type ContainingBags = M.Map BagColor [BagColor]

-- Turn a list of Contains into a map from contained to containing colors.
-- For this the number of containing bags are irrelevant.
mapContainingBags :: [Contain] -> ContainingBags
mapContainingBags = foldl' f M.empty
 where
  f :: ContainingBags -> Contain -> ContainingBags
  f m (Contain outer inner _) = M.insertWith (++) inner [outer] m

-- Creates a set of all the bag colors which must contain the given color.
-- Does this by performing a DFS on the graph defined by a ContainingBags,
-- starting with the bags which directly contain the given color.
setEventuallyContainingBags :: ContainingBags -> BagColor -> S.Set BagColor
setEventuallyContainingBags m b = f S.empty (m M.! b)
 where
  f :: S.Set BagColor -> [BagColor] -> S.Set BagColor
  f s q = case q of
    []     -> s  -- no more bags to look at
    c : cs -> if S.member c s  -- if we've already searched from here
      then f s cs       -- then discard and move on
      else f (S.insert c s) $ fromMaybe [] (M.lookup c m) ++ cs
      -- else remember and discard c, and look at all bags containing C

-- Tracks how many bags of each color a bag of a given color must contain.
-- Bag colors which contain no other bags do not appear.
type ContainBagCounts = M.Map BagColor [(BagColor, Int)]

-- Construct a ContainBagCounts from the parsed Contains.
mapContainBagCounts :: [Contain] -> ContainBagCounts
mapContainBagCounts = foldl' f M.empty
 where
  f :: ContainBagCounts -> Contain -> ContainBagCounts
  f m (Contain outer inner num) = M.insertWith (++) outer [(inner, num)] m

-- Calculates how many bags a given bag color must contain.
-- Does this by DFS on a ContainBagCounts, and only calculates for a given
-- bag color and the bag colors it eventually contains.
mapEventualContainBagCounts
  :: ContainBagCounts -> BagColor -> M.Map BagColor Int
mapEventualContainBagCounts m color = f M.empty [(color, True)]
 where
  f :: M.Map BagColor Int -> [(BagColor, Bool)] -> M.Map BagColor Int
  f result []               = result
  -- We already calculated the result for this color, so discard.
  f result ((c, _) : cs) | M.member c result = f result cs
  -- True means that counts for the immediately contained colors haven't been
  -- calculated yet (as far as we know), so put them on the stack.  When we get
  -- back to this color, we'll have calculated the necessary counts, so replace
  -- c with a note to that effect.
  f result ((c, True) : cs) = f
    result
    (  map (\(c', _) -> (c', True)) (M.findWithDefault [] c m)
    ++ ((c, False) : cs)
    )
  -- We need to calculate the count for this color, but all the necessary
  -- subcolors (which might be none) have been calculated.  So calculate
  -- and insert the result into the returned map.
  f result ((c, _) : cs) = f (M.insert c cnt result) cs
   where
    cnt =
      sum $ map (\(c', n) -> n * (1 + result M.! c')) (M.findWithDefault [] c m)

day07 :: String -> String
day07 input =
  "Eventually containing colors: "
    ++ show (S.size colors)
    ++ "\n"
    ++ "Contained in shiny gold bag: "
    ++ show (eventualContainCounts M.! "shiny gold")
    ++ "\n"
 where
  containsList  = listContainsLines $ lines input
  containingMap = mapContainingBags containsList
  colors        = setEventuallyContainingBags containingMap "shiny gold"
  containCounts = mapContainBagCounts containsList
  eventualContainCounts =
    mapEventualContainBagCounts containCounts "shiny gold"
