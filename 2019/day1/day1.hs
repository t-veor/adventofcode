#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
module Day1 where

import System.Environment (getArgs)

fuelReqs :: Int -> Int
fuelReqs x = x `quot` 3 - 2

realFuelReqs :: Int -> Int
realFuelReqs x
  | x < 0     = 0
  | otherwise = next + realFuelReqs next
  where next = x `quot` 3 - 2

star1 :: [Int] -> Int
star1 = sum . map fuelReqs

star2 :: [Int] -> Int
star2 = sum . map realFuelReqs

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- map read . lines <$> readFile filename
  print $ star1 input
  print $ star2 input
