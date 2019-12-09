#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Control.Monad.ST.Lazy
import Data.Array
import Data.Array.ST
import Data.STRef.Lazy
import Data.List (maximum, permutations)

runProgram :: Array Int Int -> [Int] -> [Int]
runProgram arr i = runST $ do
  memory <- thaw arr
  inputs <- newSTRef i
  runProgram_ memory inputs

runProgram_
  :: forall s. STArray s Int Int
  -> STRef s [Int]
  -> ST s [Int]
runProgram_ memory inputs = exec 0
  where
    exec pc = do
      (opcode, (src, trg, dst), (op1, op2, op3)) <- fetchDecode pc
      case opcode of
        99 -> return []
        1  -> do
          writeMem dst (op1 + op2)
          exec (pc + 4)
        2  -> do
          writeMem dst (op1 * op2)
          exec (pc + 4)
        3  -> do
          val <- readInput
          writeMem src val
          exec (pc + 2)
        4  -> do
          (op1:) <$> exec (pc + 2)
        5  -> exec (if op1 /= 0 then op2 else pc + 3)
        6  -> exec (if op1 == 0 then op2 else pc + 3)
        7  -> do
          writeMem dst (fromEnum $ op1 < op2)
          exec (pc + 4)
        8  -> do
          writeMem dst (fromEnum $ op1 == op2)
          exec (pc + 4)
        _  -> fail $ "Unknown opcode " ++ show opcode
    readMem :: Int -> ST s Int
    readMem loc = do
      (begin, end) <- getBounds memory
      if begin <= loc && loc <= end then readArray memory loc else return 0
    writeMem :: Int -> Int -> ST s ()
    writeMem loc x = writeArray memory loc x
    fetchDecode pc = do
      op <- readMem pc
      let opcode = op `mod` 100
      let srcMode = (op `mod` 1000) `quot` 100
      let trgMode = (op `mod` 10000) `quot` 1000
      let dstMode = op `quot` 100000
      src <- readMem (pc + 1)
      trg <- readMem (pc + 2)
      dst <- readMem (pc + 3)
      op1 <- if srcMode == 0 then readMem src else return src
      op2 <- if trgMode == 0 then readMem trg else return trg
      op3 <- if dstMode == 0 then readMem dst else return dst
      return (opcode, (src, trg, dst), (op1, op2, op3))
    readInput = do
      i <- readSTRef inputs
      case i of
        []     -> return 0
        (x:xs) -> writeSTRef inputs xs >> return x

runAmplifiers :: Array Int Int -> [Int] -> Int
runAmplifiers prog settings = go settings [0]
  where go (s:ss) xs = go ss $ runProgram prog (s:xs)
        go []     xs = head xs

-- through the magic of haskell we do the loop by making a recursive binding!
-- lol wtf this actually just works with the lazy ST monad
runAmplifierLoop :: Array Int Int -> [Int] -> Int
runAmplifierLoop prog settings = let xs = go settings (0:xs) in last xs
  where go (s:ss) xs = go ss $ runProgram prog (s:xs)
        go []     xs = xs

star1 :: Array Int Int -> Int
star1 prog = maximum $ map (runAmplifiers prog) $ permutations [0..4]

star2 :: Array Int Int -> Int
star2 prog = maximum $ map (runAmplifierLoop prog) $ permutations [5..9]

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
