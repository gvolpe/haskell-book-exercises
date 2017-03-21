module Main where

import System.IO
import Hello

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  sayHello name
