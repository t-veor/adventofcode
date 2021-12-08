#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)
import Data.List (transpose)

countBits :: [Bool] -> (Int, Int)
countBits = go 0 0
  where go z o (True  : xs) = go z (o + 1) xs
        go z o (False : xs) = go (z + 1) o xs
        go z o []           = (z, o)

fromBinary :: [Bool] -> Int
fromBinary = go 0
  where go n (x:xs) = go (2 * n + fromEnum x) xs
        go n []     = n

filterByBit :: Bool -> Int -> [[Bool]] -> [[Bool]]
filterByBit negate n xs = filter ((== criteria) . (!! n)) xs
  where (zeroes, ones) = countBits $ map (!! n) xs
        criteria = negate /= (ones >= zeroes)

filterUntilOne :: Bool -> [[Bool]] -> [Bool]
filterUntilOne negate = go 0
  where go n xs@(_:_:_) = go (n + 1) $ filterByBit negate n xs
        go _ [x]        = x
        go _ []         = error "unreachable"

star1 :: [[Bool]] -> Int
star1 xs = fromBinary gamma * fromBinary epsilon
  where counts = map countBits $ transpose xs
        gamma = map (\(zeroes, ones) -> ones >= zeroes) counts
        epsilon = map not gamma

star2 :: [[Bool]] -> Int
star2 xs = fromBinary oxygen * fromBinary co2
  where oxygen = filterUntilOne True xs
        co2 = filterUntilOne False xs

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- map (map (=='1')) . lines <$> readFile filename
  print $ star1 input
  print $ star2 input
