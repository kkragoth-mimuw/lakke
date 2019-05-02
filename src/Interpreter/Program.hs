module Interpreter.Program where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import Debug.Trace
import Debug.Pretty.Simple (pTraceShowM)
import System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)

import AbsLakke
import Interpreter.Domains
import Interpreter.ErrorTypes
import Interpreter.EvalMonad
import Interpreter.Semantics.TopDefs

runProgram :: (Executable program) => Env -> Store -> program -> ((Either RuntimeError (), Store), [String])
runProgram env st program = runWriter (runStateT (runExceptT (runReaderT (exec program) env)) st) 

class Executable f where
  exec :: f -> Eval ()

instance Executable Program where
  exec (Program topdefs) = do
    env <- evalTopDefs topdefs
    state <- get

    debug env state

    case (env & (funcsEnv & view)) ^.at (Ident "main") of
      Nothing -> throwError $ RErrorNoMainFunction
      Just loc -> case (state & (funcDefs & view)) ^.at loc of
        Nothing -> throwError $ RErrorNoMainFunction
        Just func -> return ()

debug :: Env -> Store -> Eval ()
debug env state = do
  traceM $ color Yellow "Debug"
  pTraceShowM $ state
  pTraceShowM $ env