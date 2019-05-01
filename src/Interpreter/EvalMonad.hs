module Interpreter.EvalMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Interpreter.Domains
import Interpreter.ErrorTypes

type Eval a = (ReaderT Env (ExceptT RuntimeError (StateT Store (Writer [String])))) a