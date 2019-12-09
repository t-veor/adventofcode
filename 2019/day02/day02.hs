#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Control.Monad.ST
import Data.Array
import Data.Array.ST

-- Working with arrays turns out to be horrible in Haskell, as usual

runProgram :: Array Int Int -> (Int, Int) -> Int
runProgram arr (arg1, arg2) = runST $ do
  memory <- thaw arr
  writeArray memory 1 arg1
  writeArray memory 2 arg2
  exec memory 0
  where
    exec :: STArray s Int Int -> Int -> ST s Int
    exec memory pc = do
      opcode <- readArray memory pc
      case opcode of
        99 -> readArray memory 0
        1  -> do
          (op1, op2, dst) <- load memory pc
          writeArray memory dst (op1 + op2)
          exec memory (pc + 4)
        2 -> do
          (op1, op2, dst) <- load memory pc
          writeArray memory dst (op1 * op2)
          exec memory (pc + 4)
        _ -> fail $ "Unknown opcode " ++ show opcode
    load memory pc = do
      src <- readArray memory (pc + 1)
      trg <- readArray memory (pc + 2)
      dst <- readArray memory (pc + 3)
      op1 <- readArray memory src
      op2 <- readArray memory trg
      return (op1, op2, dst)

star1 :: Array Int Int -> Int
star1 inputArray = runProgram inputArray (12, 2)

star2 :: Array Int Int -> Int
star2 inputArray = 100 * noun + verb
  where
    target = 19690720
    candidates = filter ((== target) . runProgram inputArray) [(x, y) | x <- [0..99], y <- [0..99]]
    (noun, verb) = head candidates

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- map read . splitOn "," <$> readFile filename
  let inputArray = listArray (0, length input - 1) input
  print $ star1 inputArray
  print $ star2 inputArray
