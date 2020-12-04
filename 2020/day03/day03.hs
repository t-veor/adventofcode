#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)
import qualified Data.Vector as V

type TreeGrid = V.Vector (V.Vector Bool)

parseTrees :: String -> TreeGrid
parseTrees = V.fromList . map (V.fromList . map (=='#')) . lines

countTrees :: TreeGrid -> (Int, Int) -> Int
countTrees grid (dx, dy) = go (0, 0) 0
  where go (x, y) acc
          | y >= V.length grid = acc
          | otherwise          =
              let row    = grid V.! y
                  isTree = row V.! (x `mod` V.length row)
                  acc' = if isTree then acc + 1 else acc
              in go (x + dx, y + dy) acc'

star1 :: TreeGrid -> Int
star1 = flip countTrees (3, 1)

star2 :: TreeGrid -> Int
star2 grid = product $ map (countTrees grid) $ angles
  where angles = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- parseTrees <$> readFile filename
  print $ star1 input
  print $ star2 input
