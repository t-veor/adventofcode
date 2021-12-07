#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)

pairs :: [a] -> [(a, a)]
pairs (x:xs) = zip (x:xs) xs
pairs _      = []

triples :: [a] -> [(a, a, a)]
triples (x:y:xs) = zip3 (x:y:xs) (y:xs) xs
triples _        = []

tripleSum :: (Int, Int, Int) -> Int
tripleSum (a, b, c) = a + b + c

star1 :: [Int] -> Int
star1 = sum . map (fromEnum . uncurry (<)) . pairs

star2 :: [Int] -> Int
star2 = star1 . map tripleSum . triples

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

