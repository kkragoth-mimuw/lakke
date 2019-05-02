module Interpreter.Semantics.Statements where

import           Control.Monad.Reader
import           Control.Monad.Writer

import           AbsLakke

import           Debug.Trace
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions
import           Interpreter.Values

evalStmts :: [Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (x:xs) = do
    env <- evalStmt x
    local (const env) (evalStmts xs)

evalStmt :: Stmt -> Eval Env

evalStmt (PrintLn expr) = do
    e <- evalExpr expr
    tell [show e]
    ask
