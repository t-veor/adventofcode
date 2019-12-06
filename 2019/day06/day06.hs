#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
module Day06 where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (unfoldr)

dfs :: (Ord a, Show a) => (a -> [a]) -> a -> a -> Maybe Int
dfs adj start end = case go [start] 0 S.empty of
    Right x -> Just x
    Left _  -> Nothing
  where
    go (x:xs) d visited
      | x == end           = Right d
      | S.member x visited = go xs d visited
      | otherwise          = case go (adj x) (d + 1) (S.insert x visited) of
          Right x       -> Right x
          Left visited' -> go xs d visited'
    go [] _ visited = Left visited

adjacencyLists :: (Ord a, Show a) => [(a, a)] -> (a -> [a])
adjacencyLists xs = flip (M.findWithDefault []) o
  where m = M.fromListWith (++) $ map (\(x, y) -> (x, [y])) xs
        n = M.fromListWith (++) $ map (\(x, y) -> (y, [x])) xs
        o = M.unionWith (++) m n

orbiting :: Ord a => [(a, a)] -> M.Map a a
orbiting xs = M.fromList $ map (\(x, y) -> (y, x)) xs

star1 :: Ord a => M.Map a a -> Int
star1 m = sum . map countOrbits . M.keys $ m
  where countOrbits = length . unfoldr (((\x -> (x, x)) <$>) . (m M.!?))

star2 :: (String -> [String]) -> Int
star2 adj = case dfs adj "YOU" "SAN" of
  Just x  -> x - 2
  Nothing -> error "No path between YOU and SAN found"

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- map ((\(x:y:_) -> (x, y)) . splitOn ")") . lines <$> readFile filename
  let o = orbiting input
  let adj = adjacencyLists input
  print $ star1 o
  print $ star2 adj
