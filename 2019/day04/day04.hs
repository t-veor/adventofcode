#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
module Day04 where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (group)

monotonic :: (Ord a) => [a] -> Bool
monotonic xs = all (uncurry (<=)) $ zip xs (tail xs)

hasConsecutive :: (Eq a) => [a] -> Bool
hasConsecutive = any ((> 1) . length) . group

hasAdjacent :: (Eq a) => [a] -> Bool
hasAdjacent = any ((== 2) . length) . group

star1 :: (Ord a, Eq a) => [[a]] -> Int
star1 = length . filter hasConsecutive . filter monotonic

star2 :: (Ord a, Eq a) => [[a]] -> Int
star2 = length . filter hasAdjacent . filter monotonic

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  [start, end] <- map (read :: String -> Int) . splitOn "-" <$> readFile filename
  let input = map show [start..end]
  print $ star1 input
  print $ star2 input
