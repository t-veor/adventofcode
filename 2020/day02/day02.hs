#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Password = Password
  { first :: Int
  , second :: Int
  , req :: Char
  , password :: String
  } deriving (Show)

parsePassword :: Parser Password
parsePassword = do
  f <- read <$> many digit
  char '-'
  s <- read <$> many digit
  spaces
  r <- anyChar
  char ':'
  spaces
  p <- many $ noneOf "\n"
  return $ Password f s r p

parsePasswords :: Parser [Password]
parsePasswords = many $ parsePassword <* optional newline

star1 :: [Password] -> Int
star1 = length . filter isValid
  where count y = foldl (\acc x -> if x == y then acc + 1 else acc) 0
        isValid p =
          let c = count (req p) (password p)
          in first p <= c && c <= second p

star2 :: [Password] -> Int
star2 = length . filter isValid
  where isValid p =
          let f = password p !! (first p - 1) == req p
              s = password p !! (second p - 1) == req p
          in f && not s || not f && s

main :: IO ()
main = do
  args <- getArgs
  let
    filename = case args of
      (x:_) -> x
      _     -> "input.txt"
  (Right input) <- parse parsePasswords "" <$> readFile filename
  print $ star1 input
  print $ star2 input
