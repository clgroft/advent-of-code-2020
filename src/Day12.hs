-- | Solutions to day 12

module Day12
  ( day12
  ) where

data CompassDirection = N | E | S | W deriving Eq
data Rotation = R | L deriving Eq
data Direction = Compass CompassDirection | Rotate Rotation | Forward deriving Eq
data Instruction = Instruction
  { direction :: Direction
  , magnitude :: Int
  }

parseDirection :: Char -> Direction
parseDirection 'N' = Compass N
parseDirection 'E' = Compass E
parseDirection 'S' = Compass S
parseDirection 'W' = Compass W
parseDirection 'L' = Rotate L
parseDirection 'R' = Rotate R
parseDirection 'F' = Forward
parseDirection c   = error ("illegal direction " ++ show c)

parseLine :: String -> Instruction
parseLine []          = error "empty line"
parseLine (dir : mag) = Instruction (parseDirection dir) (read mag)

turnRight :: Int -> CompassDirection -> CompassDirection
turnRight 90  N = E
turnRight 90  E = S
turnRight 90  S = W
turnRight 90  W = N
turnRight 180 N = S
turnRight 180 E = W
turnRight 180 S = N
turnRight 180 W = E
turnRight 270 N = W
turnRight 270 E = N
turnRight 270 S = E
turnRight 270 W = S
turnRight _   _ = error "illegal angle"

turnLeft :: Int -> CompassDirection -> CompassDirection
turnLeft 90  N = W
turnLeft 90  E = N
turnLeft 90  S = E
turnLeft 90  W = S
turnLeft 180 N = S
turnLeft 180 E = W
turnLeft 180 S = N
turnLeft 180 W = E
turnLeft 270 N = E
turnLeft 270 E = S
turnLeft 270 S = W
turnLeft 270 W = N
turnLeft _   _ = error "illegal angle"

data ShipState = ShipState
  { x, y        :: Int
  , orientation :: CompassDirection
  }

initialState :: ShipState
initialState = ShipState 0 0 E

follow :: ShipState -> Instruction -> ShipState
follow ss i = case direction i of
  Compass N -> ss { y = y ss + magnitude i }
  Compass E -> ss { x = x ss + magnitude i }
  Compass S -> ss { y = y ss - magnitude i }
  Compass W -> ss { x = x ss - magnitude i }
  Rotate  R -> ss { orientation = turnRight (magnitude i) (orientation ss) }
  Rotate  L -> ss { orientation = turnLeft (magnitude i) (orientation ss) }
  Forward   -> follow ss (i { direction = Compass (orientation ss) })

manhattanNorm :: ShipState -> Int
manhattanNorm ss = abs (x ss) + abs (y ss)

data ShipState' = ShipState'
  { px, py :: Int
  , wx, wy :: Int
  }

initialState' :: ShipState'
initialState' = ShipState' 0 0 10 1

follow' :: ShipState' -> Instruction -> ShipState'
follow' ss i = case direction i of
  Compass N -> ss { wy = wy ss + magnitude i }
  Compass E -> ss { wx = wx ss + magnitude i }
  Compass S -> ss { wy = wy ss - magnitude i }
  Compass W -> ss { wx = wx ss - magnitude i }
  Rotate  R -> case magnitude i of
    90  -> ss { wx = wy ss, wy = -(wx ss) }
    180 -> ss { wx = -(wx ss), wy = -(wy ss) }
    270 -> ss { wx = -(wy ss), wy = wx ss }
    _   -> error "invalid angle"
  Rotate L -> case magnitude i of
    90  -> ss { wx = -(wy ss), wy = wx ss }
    180 -> ss { wx = -(wx ss), wy = -(wy ss) }
    270 -> ss { wx = wy ss, wy = -(wx ss) }
    _   -> error "invalid angle"
  Forward -> ss { px = px ss + (magnitude i * wx ss)
                , py = py ss + (magnitude i * wy ss)
                }

manhattanNorm' :: ShipState' -> Int
manhattanNorm' ss = abs (px ss) + abs (py ss)

day12 :: String -> String
day12 input = output where
  instructions   = map parseLine $ lines input
  finalState     = foldl follow initialState instructions
  finalDistance  = manhattanNorm finalState
  finalState'    = foldl follow' initialState' instructions
  finalDistance' = manhattanNorm' finalState'
  output =
    "Final distance: "
      ++ show finalDistance
      ++ "\n"
      ++ "True final distance: "
      ++ show finalDistance'
      ++ "\n"
