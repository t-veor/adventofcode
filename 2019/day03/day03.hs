#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
module Day03 where

import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List (minimum)
import Data.Function (on)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Dir = U | D | L | R

parseDir :: Parser Dir
parseDir =   (char 'U' >> return U)
         <|> (char 'D' >> return D)
         <|> (char 'L' >> return L)
         <|> (char 'R' >> return R)

parseSeg :: Parser (Dir, Int)
parseSeg = do
  d <- parseDir
  i <- read <$> many digit
  return (d, i)

parseWire :: Parser [(Dir, Int)]
parseWire = parseSeg `sepBy` char ','

delta :: Dir -> (Int, Int)
delta U = ( 0, -1)
delta D = ( 0,  1)
delta L = (-1,  0)
delta R = ( 1,  0)

wirePositions :: [(Dir, Int)] -> M.Map (Int, Int) Int
wirePositions = M.fromList . reverse . go 0 0 0
  where go _ _ _ [] = []
        go x y s ((_, 0):w) = go x y s w
        go x y s ((d, n):w) = ((x', y'), s') : go x' y' s' ((d, n - 1):w)
          where (dx, dy) = delta d
                x' = x + dx
                y' = y + dy
                s' = s + 1

star1 :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int -> Int
star1 ma mb = minimum . map snd . M.toList $ dists
  where dists = M.intersectionWithKey (\(x, y) _ _ -> abs x + abs y) ma mb

star2 :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int -> Int
star2 ma mb = minimum . map snd . M.toList $ steps
  where steps = M.intersectionWith (+) ma mb

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  [Right a, Right b] <- map (parse parseWire "") . lines <$> readFile filename
  let ma = wirePositions a
  let mb = wirePositions b
  print $ star1 ma mb
  print $ star2 ma mb
