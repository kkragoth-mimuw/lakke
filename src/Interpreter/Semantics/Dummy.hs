module Interpreter.Semantics.Dummy where

import           Control.Lens                       hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe

import           AbsLakke

import           Interpreter.Debug
import           Interpreter.DomainsUtils
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Declarations
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Statements

evalStmts :: [Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (x:xs) = do
    env <- evalStmtOrDeclaration x
    local (const env) (evalStmts xs)