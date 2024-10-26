module Interpreter (interpret, interpretFile) where

import           Control.Monad         
import           Prelude             
import           System.Exit           
import           System.IO               

import           Evaluator
import           ParWyso    (myLexer, pProgram)
import           AbsWyso
import           Typechecker

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpret :: String -> IO ()
interpret input = do
  let tokens = myLexer input
  let parsedTokens = pProgram tokens
  typecheckAndEvaluate parsedTokens

typecheckAndEvaluate :: Either String Program -> IO ()
typecheckAndEvaluate (Left err) = do
  hPrint stderr err
  exitFailure
typecheckAndEvaluate (Right tree) =
  case checkType tree of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right _ -> do
      evaluated <- evalProgram tree
      case evaluated of
        Left err -> do
          hPrint stderr err
          exitFailure
        Right _ -> exitSuccess
