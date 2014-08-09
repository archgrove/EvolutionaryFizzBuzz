module Main where

import System.Environment
import System.Random
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace

-- Utilities
changeIndex :: Int -> a -> [a] -> [a]
changeIndex i e l = (take i l) ++ (e : (drop (i + 1) l))

removeIndex :: Int -> [a] -> [a]
removeIndex i l = (take i l) ++ (drop (i + 1) l)

randomInt :: Int -> IO Int
randomInt i = (getStdRandom (randomR (1 :: Int, i)))

data Instruction =   PushText String
                   | PushNumber Int
                   | Pop
                   | JumpGreater
                   | JumpLesser
                   | PrintLine
                   | Add
                   | Subtract
                   | Modulo
                   | Concatenate
                   | Copy
                   | Jump
                   deriving Show

data Value =   TextValue String
             | NumberValue Int
             deriving Show

type Stack = [Value]
type PC = Int
type Program = [Instruction]
type Error = String
type Output = [String]

fizzBuzz :: Program
fizzBuzz = [PushNumber 1, -- 0
  Copy, -- 1
  PushNumber 15, -- 2
  Modulo, -- 3
  PushNumber 0, -- 4
  PushNumber 11, -- 5 
  JumpGreater, -- 6
  PushText "FizzBuzz", -- 7
  PrintLine, -- 8
  PushNumber 33, -- 9
  Jump, -- 10
  Copy, -- 11
  PushNumber 5, -- 12
  Modulo, -- 13
  PushNumber 0, -- 14
  PushNumber 21, -- 15
  JumpGreater, -- 16
  PushText "Buzz", -- 17
  PrintLine, -- 18
  PushNumber 33, -- 19
  Jump, -- 20
  Copy, -- 21
  PushNumber 3, -- 22
  Modulo, -- 23
  PushNumber 0, -- 24
  PushNumber 31, -- 25
  JumpGreater, -- 26
  PushText "Fizz", -- 27
  PrintLine, -- 28
  PushNumber 33, -- 29
  Jump, -- 30
  Copy, -- 31
  PrintLine, --32
  PushNumber 1, -- 33
  Add, -- 34
  Copy, -- 35
  PushNumber 100, -- 37
  PushNumber 1, -- 38
  JumpLesser] -- 39

step :: Instruction -> PC -> Stack -> Either (Maybe String, PC, Stack) Error
step (PushText val) pc s = Left (Nothing, pc + 1, (TextValue val) : s)
step (PushNumber val) pc s = Left (Nothing, pc + 1, (NumberValue val) : s)
step Pop pc s = case s of
  [] -> Right "Cannot pop the empty stack"
  v : rest -> Left (Nothing, pc + 1, rest)
step JumpGreater pc s = case s of
  (NumberValue v3) : (NumberValue v2) : NumberValue (v1) : rest -> 
    if v1 > v2 then Left (Nothing, v3, rest) else Left(Nothing, pc + 1, rest)
  _ -> Right "Jump without 3 integer operands"
step JumpLesser pc s = case s of
  (NumberValue v3) : (NumberValue v2) : NumberValue (v1) : rest -> 
    if v1 < v2 then Left (Nothing, v3, rest) else Left(Nothing, pc + 1, rest)
  _ -> Right "Jump without 3 integer operands"
step PrintLine pc s = case s of
  [] -> Right "Cannot print from the empty stack"
  (NumberValue v) : rest -> Left (Just (show v), pc + 1, rest)
  (TextValue v) : rest -> Left (Just v, pc + 1, rest)
step Add pc s = case s of
  (NumberValue v1) : (NumberValue v2) : rest -> 
    Left (Nothing, pc + 1, NumberValue (v1 + v2) : rest)
  _ -> Right "Add without 2 integer operands"
step Subtract pc s = case s of
  (NumberValue v1) : (NumberValue v2) : rest -> 
    Left (Nothing, pc + 1, NumberValue (v1 - v2) : rest)
  _ -> Right "Subtract without 2 integer operands"
step Modulo pc s = case s of
  (NumberValue v2) : (NumberValue v1) : rest | v2 > 0 -> 
    Left (Nothing, pc + 1, NumberValue (mod v1 v2) : rest)
  _ -> Right "Modulo without 2 integer operands, the second positive"
step Concatenate pc s = case s of
  (TextValue v1) : (TextValue v2) : rest -> 
    Left (Nothing, pc + 1, TextValue (v1 ++ v2) : rest)
  _ -> Right "Concatenate without 2 string operands"
step Copy pc s = case s of
  v : rest -> 
    Left (Nothing, pc + 1, v : v : rest)
  all -> Left (Nothing, pc + 1, all)
step Jump pc s = case s of
  (NumberValue v) : rest -> 
    Left (Nothing, v, rest)
  all -> Left (Nothing, pc + 1, all)

execute :: Program -> Either Output Error
-- Execute 500 steps of execution
execute p = executeWithState 500 p 0 [] []
  where
  executeWithState :: Int -> Program -> PC ->
                      Stack -> Output -> Either Output Error
--  executeWithState steps p pc s o | trace ((show pc) ++ " : " ++ (show s)) False = undefined
  executeWithState steps p pc s o = 
    -- If the program counter is off the program, terminate with output
    -- Only allow steps execution steps
    if steps == 0 then Left o else
      if pc >= length p || pc < 0 then
        Left o
      else 
        case (step (p !! pc) pc s) of
          Left (result, newPc, newStack) -> case result of 
            Just s -> executeWithState (steps - 1) p newPc newStack (s : o)
            Nothing -> executeWithState (steps - 1) p newPc newStack o
          Right error -> Right error

randomProgram :: IO Program
randomProgram = do
  s <- randomInt 20
  p <- (randomProgramOfLength s)
  return p

randomProgramOfLength :: Int -> IO Program
randomProgramOfLength 0 = do
  i <- randomInstruction
  return [ i ]
randomProgramOfLength n = do
  p <- randomProgramOfLength (n - 1)
  i <- randomInstruction
  return (i : p)

randomInstruction :: IO Instruction
randomInstruction = do
  s <- randomInt 22
  i <- randomNum
  v <- randomString
  return (case s of
     x | x > 0 && x <= 6 -> PushText v
     x | x > 6 && x <= 12 -> PushNumber i
     13 -> Pop
     14 -> JumpGreater
     15 -> JumpLesser
     16 -> PrintLine
     17 -> Add
     18 -> Subtract
     19 -> Modulo
     20 -> Concatenate
     21 -> Copy
     22 -> Jump)

randomNum :: IO Int
randomNum = do
  s <- (getStdRandom (randomR (-128 :: Int, 128 :: Int)))
  return s

randomString :: IO String
randomString = do
  i <- randomInt 3
  return (case i of
    1 -> "Fizz"
    2 -> "Buzz"
    3 -> "FizzBuzz")

breed :: Program -> Program -> Program
breed p1 p2 = p1

mutate :: Program -> IO Program
mutate p1 = do
  i <- randomInt 100
  case i of
    -- Change a random instruction
    x | x >= 0 && x <= 98 -> do
     at <- randomInt ((length p1) - 1)
     new <- randomInstruction
     return (changeIndex at new p1)
    -- Delete a random instruction
    x | x == 99 -> do
     at <- randomInt ((length p1) - 1)
     return (removeIndex at p1)
    -- Add a random instruction
    x -> do
      i <- randomInstruction
      return (i : p1)

compete :: Program -> Program -> Program
compete p1 p2 = 
  let r1 = execute p1 in
  let r2 = execute p2 in
  case (r1, r2) of
    (Right _, Right _) -> p1
    (Left _, Right _) -> p1
    (Right _, Left _) -> p2
    (Left o1, Left o2) -> if (length o1 > length o2) then p1 else p2

evolve :: Int -> Program -> IO Program
evolve 0 p = return p
evolve n p = do
  p1 <- mutate p
  p2 <- mutate p
  evolve (n - 1) (compete p1 p2)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ((head args) ++ " generations of FIZZ BUZZ")
  p <- randomProgram
  putStrLn (show p)
  end <- evolve (read $ head args :: Int) p
  putStrLn (show (execute end))
  putStrLn (show end)
