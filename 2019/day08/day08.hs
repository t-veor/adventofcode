-- stack --resolver lts-14.16 script
{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.List (minimumBy, dropWhile, dropWhileEnd, intercalate)
import Data.Function (on)
import Data.Char (isSpace)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

getFreqs :: Ord a => [a] -> M.Map a Int
getFreqs = M.fromListWith (+) . map (,1)

getLayers :: (Int, Int) -> [a] -> [[a]]
getLayers (w, h) = chunksOf (w * h)

composite :: String -> String -> String
composite = zipWith over
  where over t   '2' = t
        over '2' b   = b
        over t   _   = t

display :: String -> String
display = map charMap
  where
    charMap x = case x of
      '0'  -> ' '
      '1'  -> '\x2588'
      x    -> x

star1 :: [String] -> Int
star1 ls = M.findWithDefault 0 '1' m * M.findWithDefault 0 '2' m
  where m = minimumBy (compare `on` M.findWithDefault 0 '0') $ map getFreqs ls

star2 :: Int -> [String] -> String
star2 width (l:ls) = display $ intercalate "\n" $ chunksOf width final
  where final = foldl composite l ls

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  input <- trim <$> readFile filename
  let (width, height) = (25, 6)
  let layers = getLayers (width, height) input
  print $ star1 layers
  putStrLn $ star2 width layers
