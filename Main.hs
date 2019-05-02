module Main where


import           Control.Monad                 (when)
import qualified Data.Map                      as Map
import           Data.Typeable
import           System.Console.Pretty         (Color (..), Style (..), bgColor, color, style, supportsPretty)
import           System.Environment            (getArgs, getProgName)
import           System.Exit                   (exitFailure, exitSuccess)
import           System.IO                     (hGetContents, stdin)

import           AbsLakke
import           LexLakke
import           ParLakke
import           PrintLakke
import           SkelLakke

import           Interpreter.Program
import           Interpreter.Semantics.Domains (initEnv, initStore)


import           ErrM

type ParseFun = [Token] -> Err Program

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do let ((result, _), buffer) = runProgram initEnv initStore tree
                          putStr $ unlines buffer

                          case result of
                            Left error -> putStrLn (color Red ("Lakke has encountered a problem: " ++ show error))
                            _          -> exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pProgram
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs





