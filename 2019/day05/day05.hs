#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
{-# LANGUAGE FlexibleContexts #-}

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef

-- Working with arrays turns out to be horrible in Haskell, as usual

runProgram :: Array Int Int -> [Int] -> [Int]
runProgram arr i = runST $ do
  memory <- thaw arr
  inputs <- newSTRef i
  outputs <- newSTRef []
  runProgram_ memory inputs outputs

runProgram_
  :: STUArray s Int Int
  -> STRef s [Int]
  -> STRef s [Int]
  -> ST s [Int]
runProgram_ memory inputs outputs = exec 0
  where
    exec pc = do
      (opcode, (src, trg, dst), (op1, op2, op3)) <- fetchDecode pc
      case opcode of
        99 -> reverse <$> readSTRef outputs
        1  -> do
          write dst (op1 + op2)
          exec (pc + 4)
        2  -> do
          write dst (op1 * op2)
          exec (pc + 4)
        3  -> do
          val <- readInput
          write src val
          exec (pc + 2)
        4  -> do
          writeOutput op1
          exec (pc + 2)
        5  -> exec (if op1 /= 0 then op2 else pc + 3)
        6  -> exec (if op1 == 0 then op2 else pc + 3)
        7  -> do
          write dst (fromEnum $ op1 < op2)
          exec (pc + 4)
        8  -> do
          write dst (fromEnum $ op1 == op2)
          exec (pc + 4)
        _  -> fail $ "Unknown opcode " ++ show opcode
    read loc = do
      (begin, end) <- getBounds memory
      if begin <= loc && loc <= end then readArray memory loc else return 0
    write loc x = writeArray memory loc x
    fetchDecode pc = do
      op <- read pc
      let opcode = op `mod` 100
      let srcMode = (op `mod` 1000) `quot` 100
      let trgMode = (op `mod` 10000) `quot` 1000
      let dstMode = op `quot` 100000
      src <- read (pc + 1)
      trg <- read (pc + 2)
      dst <- read (pc + 3)
      op1 <- if srcMode == 0 then read src else return src
      op2 <- if trgMode == 0 then read trg else return trg
      op3 <- if dstMode == 0 then read dst else return dst
      return (opcode, (src, trg, dst), (op1, op2, op3))
    readInput = do
      i <- readSTRef inputs
      case i of
        []     -> return 0
        (x:xs) -> writeSTRef inputs xs >> return x
    writeOutput x = modifySTRef' outputs (x:)

star1 :: Array Int Int -> Int
star1 inputArray = last $ runProgram inputArray [1]

star2 :: Array Int Int -> Int
star2 inputArray = head $ runProgram inputArray [5]

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
