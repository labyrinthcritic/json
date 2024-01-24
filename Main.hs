module Main where

import Data.List
import System.Environment
import System.Exit
import System.IO

import Parse

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      contents <- readFile (args !! 0)
      let structure = parse value contents
      print structure
    else do
      hPutStrLn stderr "usage: parse <path>"
      exitFailure
