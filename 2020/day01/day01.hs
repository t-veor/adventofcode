#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)

star1 :: [Int] -> Int
star1 input = head [x * y | x <- input, y <- input, x + y == 2020]

star2 :: [Int] -> Int
star2 input = head [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]

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
