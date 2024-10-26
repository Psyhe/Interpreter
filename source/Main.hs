module Main where

import           Interpreter
import           Prelude        

import           System.Environment
import           System.Exit      


main :: IO ()
main = do
  args <- getArgs
  case args of
    [f]        -> interpretFile f
    []         -> interpretStdin
    _          -> invalidUsage

interpretStdin :: IO ()
interpretStdin = do
  getContents >>= interpret


invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid usage! \nCorrect usage: ./interpreter <path_to_file> \nOr ./interpreter <write in stdin> "
  exitFailure