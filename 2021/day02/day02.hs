#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

parseCommand :: Parser Command
parseCommand = do
  cty <- parseCommandType
  space
  n <- read <$> many digit
  return $ cty n
  where parseCommandType =   (string "forward" *> return Forward)
                         <|> (string "down"    *> return Down)
                         <|> (string "up"      *> return Up)

parseCommands :: Parser [Command]
parseCommands = many $ parseCommand <* optional newline

star1 :: [Command] -> Int
star1 = go 0 0
  where go x y (Forward n : cs) = go (x + n) y cs
        go x y (Down n    : cs) = go x (y + n) cs
        go x y (Up n      : cs) = go x (y - n) cs
        go x y []               = x * y

star2 :: [Command] -> Int
star2 = go 0 0 0
  where go x y aim (Forward n : cs) = go (x + n) (y + n * aim) aim cs
        go x y aim (Down n    : cs) = go x y (aim + n) cs
        go x y aim (Up n      : cs) = go x y (aim - n) cs
        go x y _   []               = x * y

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  (Right input) <- parse parseCommands "" <$> readFile filename
  print $ star1 input
  print $ star2 input


