module Interpreter.Program where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Debug.Trace

import AbsLakke
import Interpreter.Domains
import Interpreter.ErrorTypes
import Interpreter.EvalMonad

runProgram :: (Executable program) => Env -> Store -> program -> ((Either RuntimeError (), Store), [String])
runProgram env st program = runWriter (runStateT (runExceptT (runReaderT (exec program) env)) st) 

class Executable f where
  exec :: f -> Eval ()

instance Executable Program where
  exec (Program topdefs) = do
    env <- evalTopDefs topdefs
    state <- get

    traceM $ "state: " ++ show state
    traceM $ "env: " ++ show env

    return ()

evalTopDefs :: [TopDef] -> Eval Env
evalTopDefs _ = undefined