-- | Solutions for day 8

module Day08 where

import           Control.Monad.State.Strict
import qualified Data.Array                    as A
import qualified Data.Set                      as S
-- import           Debug.Trace                    ( traceM )

type PC = Int
type Accumulator = Int
type VisitedLines = S.Set Int
data MachineState = MachineState PC Accumulator VisitedLines
  deriving Show

initialMachineState :: MachineState
initialMachineState = MachineState 0 0 S.empty

data Operation = Acc | Jmp | Nop deriving Show
type Argument = Int
data Instruction = Instruction Operation Argument
  deriving Show
type InstructionList = A.Array Int Instruction

parseInstruction :: String -> Instruction
parseInstruction str = Instruction op arg
 where
  [a, b] = words str
  op     = case a of
    "acc" -> Acc
    "jmp" -> Jmp
    "nop" -> Nop
    _     -> error "unable to parse operation"
  (sign : num) = b
  arg          = case sign of
    '+' -> read num
    '-' -> -(read num)
    _   -> error "unable to parse argument"

allInstructions :: String -> InstructionList
allInstructions input = A.listArray (0, length lns - 1)
  $ map parseInstruction lns
  where lns = lines input

data Termination = Normal | InfiniteLoop deriving Eq

runInstructions :: InstructionList -> State MachineState Termination
runInstructions instrs = loop
 where
  (_, lastInstruction) = A.bounds instrs
  loop                 = do
    currState <- get
    let MachineState pc acc visitedLines = currState
    -- traceM
    --   (  "PC: "
    --   ++ show pc
    --   ++ " Acc: "
    --   ++ show acc
    --   ++ " Visited: "
    --   ++ show visitedLines
    --   )
    if pc > lastInstruction
      then return Normal
      else if pc `S.member` visitedLines
        then return InfiniteLoop
        else do
          let newVisitedLines    = S.insert pc visitedLines
          let Instruction op arg = instrs A.! pc
          -- traceM $ show $ Instruction op arg
          put $ case op of
            Acc -> MachineState (pc + 1) (acc + arg) newVisitedLines
            Jmp -> MachineState (pc + arg) acc newVisitedLines
            Nop -> MachineState (pc + 1) acc newVisitedLines
          loop

changeInstruction :: InstructionList -> Int -> [InstructionList]
changeInstruction instrs n = case instrs A.! n of
  Instruction Acc _ -> []
  Instruction Jmp m -> [instrs A.// [(n, Instruction Nop m)]]
  Instruction Nop m -> [instrs A.// [(n, Instruction Jmp m)]]

firstNormalHaltAcc :: InstructionList -> Int
firstNormalHaltAcc instrs = firstAcc
 where
  changedLists = concatMap (changeInstruction instrs) [0 ..]  -- yay laziness!
  endStates =
    map (flip runState initialMachineState . runInstructions) changedLists
  normalTerminations             = filter ((== Normal) . fst) endStates
  (_, MachineState _ firstAcc _) = head normalTerminations

day08 :: String -> String
day08 input =
  "Value in accumulator (initially): "
    ++ show acc
    ++ "\n"
    ++ "Value in accumulator (first correct): "
    ++ show normalAcc
    ++ "\n"
 where
  instrs               = allInstructions input
  MachineState _ acc _ = execState (runInstructions instrs) initialMachineState
  normalAcc            = firstNormalHaltAcc instrs
