-- stack --resolver lts-14.16 script

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (concat, transpose, maximumBy)
import Data.Function (on)
import Data.Ratio

type Angle = (Bool, Maybe (Ratio Int))
{-
                                  (-y)
                                  x (False, Nothing)
                                  |
    (True, Just (1 % 1)) x        | ---->  x (False, Just (-1 % 1))
                          '.      |      .'
                         ^  '.    |    .'  |
                         |    '.  |  .'    |
                         |      '.|.'      v
              (-x) x--------------+-------------->x (+x)
(True, Just (0 % 1))     ^      .'|'.      |      (False, Just (0 % 1))
                         |    .'  |  '.    |
                         |  .'    |    '.  v
                          .'      |      '.
   (True, Just (-1 % 1)) x  <---- | <----  x (False, Just (1 % 1))
                                  v
                                  x (True, Nothing)
                                  (+y)
-}

deltaToAngle :: (Int, Int) -> Angle
deltaToAngle (x, y)
  | x == 0    = (y > 0, Nothing)
  | otherwise = (x < 0, Just $ y % x)

bucketByAngle :: (Int, Int) -> [(Int, Int)] -> M.Map Angle [(Int, Int)]
bucketByAngle xy@(x, y) = M.fromListWith (++) . map getAngle . filter (/= xy)
  where getAngle zw@(z, w) = (deltaToAngle (z - x, w - y), [zw])

stationBucket :: [(Int, Int)] -> M.Map Angle [(Int, Int)]
stationBucket xs
  = maximumBy (compare `on` M.size) $ map (flip bucketByAngle xs) xs

star1 :: M.Map Angle [(Int, Int)] -> Int
star1 = M.size

star2 :: M.Map Angle [(Int, Int)] -> Int
star2 = display . (!! 199) . concat . transpose . map snd . M.toAscList
  where display (x, y) = x * 100 + y

doubleIndex :: [[a]] -> [((Int, Int), a)]
doubleIndex
  = concat
  . map (\(row, l) -> map (\(col, x) -> ((col, row), x)) $ enumerate l)
  . enumerate
  where enumerate = enumerate_ 0
        enumerate_ _ []     = []
        enumerate_ n (x:xs) = (n, x) : enumerate_ (n + 1) xs

readInput :: String -> [(Int, Int)]
readInput = map fst . filter ((== '#') . snd) . doubleIndex . lines

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- readInput <$> readFile filename
  let bucket = stationBucket input
  print $ star1 bucket
  print $ star2 bucket

