module Main where

import System.IO ( stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import qualified Control.Exception as E

import LexCapp
import ParCapp
import SkelCapp
import PrintCapp
import AbsCapp
import AbsCapp
import Interpreter
import Core

import Data.Maybe
import ErrM


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: ./program filename"
    (filename:_) -> do
      program <- readFile filename
      case pProgram $ myLexer program of
        Bad s -> do
          putStrLn s
          exitFailure
        Ok tree -> do
          (val, store) <- runInterM initEnv initStore (runInterpreter tree)
          case val of
            Left e  -> hPutStrLn stderr e
            Right code -> hPutStrLn stderr $ "Exit code: " ++ show code
