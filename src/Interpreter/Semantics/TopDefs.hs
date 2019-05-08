module Interpreter.Semantics.TopDefs where

import           Control.Monad.Reader

import           AbsLakke
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Statements (evalDecl)

evalTopDefs :: [TopDef] -> Eval Env
evalTopDefs [] = ask
evalTopDefs (x:xs) = do
    env <- evalTopDef x
    local (const env) (evalTopDefs xs)

evalTopDef :: TopDef -> Eval Env
evalTopDef (Global decl) = evalDecl (DeclS decl)
evalTopDef (FnDef fnDef) = evalDecl (DeclF fnDef)
