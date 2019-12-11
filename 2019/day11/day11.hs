#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (maximum, minimum, intercalate)

data IntCode = IntCode
  { ic_memory :: M.Map Integer Integer
  , ic_pc :: Integer
  , ic_halted :: Bool
  , ic_rbase :: Integer
  }

type IntCodeM = State IntCode
type IntegerTriplet = (Integer, Integer, Integer)
type DecodeOut = (Integer, IntegerTriplet, IntegerTriplet)

newVM :: [Integer] -> IntCode
newVM prog = IntCode (M.fromList $ enumerate prog) 0 False 0
  where enumerate = enumerate_ 0
        enumerate_ n (x:xs) = (n, x) : enumerate_ (n + 1) xs
        enumerate_ _ []     = []

readMem :: Integer -> IntCodeM Integer
readMem loc = M.findWithDefault 0 loc <$> gets ic_memory

writeMem :: Integer -> Integer -> IntCodeM ()
writeMem loc x = modify' (\i -> i { ic_memory = M.insert loc x $ ic_memory i })

advance :: Integer -> IntCodeM ()
advance x = modify' (\i -> i { ic_pc = ic_pc i + x})

jumpTo :: Integer -> IntCodeM ()
jumpTo x = modify' (\i -> i { ic_pc = x })

getPC :: IntCodeM Integer
getPC = gets ic_pc

getRelBase :: IntCodeM Integer
getRelBase = gets ic_rbase

modifyRelBase :: Integer -> IntCodeM ()
modifyRelBase x = modify' (\i -> i { ic_rbase = ic_rbase i + x })

isHalted :: IntCodeM Bool
isHalted = gets ic_halted

setHalted :: IntCodeM ()
setHalted = modify' (\i -> i { ic_halted = True })

fetchDecode :: IntCodeM DecodeOut
fetchDecode = do
  pc <- getPC
  op <- readMem pc
  let opcode = op `mod` 100
  let srcMode = (op `mod` 1000) `quot` 100
  let trgMode = (op `mod` 10000) `quot` 1000
  let dstMode = (op `mod` 100000) `quot` 10000
  src <- readMem (pc + 1)
  trg <- readMem (pc + 2)
  dst <- readMem (pc + 3)
  (src', op1) <- decodeMode srcMode src
  (trg', op2) <- decodeMode trgMode trg
  (dst', op3) <- decodeMode dstMode dst
  return (opcode, (src', trg', dst'), (op1, op2, op3))
  where
    decodeMode mode param = case mode of
      0 -> (param,) <$> readMem param
      1 -> return (0, param)
      2 -> do
        rbase <- getRelBase
        (param + rbase,) <$> readMem (param + rbase)
      _ -> error $ "Unknown addressing mode " ++ show mode

exec :: [Integer] -> IntCodeM [Integer]
exec inputs = do
  halted <- isHalted
  if halted then
    return []
  else do
    (opcode, (src, trg, dst), (op1, op2, op3)) <- fetchDecode
    case opcode of
      99 -> return []
      1  -> do
        writeMem dst (op1 + op2)
        advance 4
        exec inputs
      2  -> do
        writeMem dst (op1 * op2)
        advance 4
        exec inputs
      3  -> case inputs of
        (x:xs) -> do
          writeMem src x
          advance 2
          exec xs
        _      -> error "No more inputs!"
      4  -> do
        advance 2
        (op1:) <$> exec inputs
      5  -> do
        if op1 /= 0 then jumpTo op2 else advance 3
        exec inputs
      6  -> do
        if op1 == 0 then jumpTo op2 else advance 3
        exec inputs
      7  -> do
        writeMem dst (fromIntegral . fromEnum $ op1 < op2)
        advance 4
        exec inputs
      8  -> do
        writeMem dst (fromIntegral . fromEnum $ op1 == op2)
        advance 4
        exec inputs
      9  -> do
        modifyRelBase op1
        advance 2
        exec inputs
      _  -> fail $ "Unknown opcode " ++ show opcode

runProgram :: IntCode -> [Integer] -> [Integer]
runProgram vm inputs = evalState (exec inputs) vm

-- Tying a knot through two functions in Haskell
runRobot :: IntCode -> Int -> (M.Map (Int, Int) Int, S.Set (Int, Int))
runRobot vm initialTile
  = let outputs = runProgram vm (fromIntegral initialTile : inputs)
        (inputs, result) = runState (go (0, 0) (0, -1) outputs) initialState
    in result
  where
    go :: (Int, Int)
       -> (Int, Int)
       -> [Integer]
       -> State (M.Map (Int, Int) Int, S.Set (Int, Int)) [Integer]
    go (x, y) (dx, dy) (color : dir : rest) = do
      (board, painted) <- get
      let board' = M.insert (x, y) (fromIntegral color) board
      let painted' = S.insert (x, y) painted
      put (board', painted')
      let (dx', dy') = case dir of
                         0 -> (dy, -dx)
                         _ -> (-dy, dx)
      let (x', y') = (x + dx', y + dy')
      let cameraOut = M.findWithDefault 0 (x', y') board'
      (fromIntegral cameraOut :) <$> go (x', y') (dx', dy') rest
    go _ _ _ = return []
    initialState = (M.singleton (0, 0) initialTile, S.empty)

showBoard :: M.Map (Int, Int) Int -> String
showBoard m
  = let minX = minimum $ map fst $ M.keys m
        maxX = maximum $ map fst $ M.keys m
        minY = minimum $ map snd $ M.keys m
        maxY = maximum $ map snd $ M.keys m
        colors = map
          (\y -> map
            (\x -> M.findWithDefault 0 (x, y) m) [minX..maxX]) [minY..maxY]
        chars = map (map charMap) colors
    in intercalate "\n" chars
  where charMap 0 = ' '
        charMap _ = '\x2588'

star1 :: [Integer] -> Int
star1 prog = S.size $ snd $ runRobot (newVM prog) 0

star2 :: [Integer] -> String
star2 prog = showBoard $ fst $ runRobot (newVM prog) 1

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- map read . splitOn "," <$> readFile filename
  print $ star1 input
  putStrLn $ star2 input


