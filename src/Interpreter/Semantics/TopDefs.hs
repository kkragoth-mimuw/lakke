module Interpreter.Semantics.TopDefs where

import           Control.Monad.Reader

import           AbsLakke
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Declarations (evalFuncDecl)
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Statements
import           Interpreter.Values

-- Note: evalDecl is imported from Interpreter.Semantics.Statements
--       and not from Interpreter.Semantics.Declarations

evalTopDefs :: [TopDef] -> Eval Env
evalTopDefs [] = ask
evalTopDefs (x:xs) = do
    env <- evalTopDef x
    local (const env) (evalTopDefs xs)

evalTopDef :: TopDef -> Eval Env
evalTopDef (Global decl)                          = evalDecl (DeclS decl)
evalTopDef fnDef@(FnDef fnType fnName args block) = evalFuncDecl $ LKFunctionDef fnType fnName args block
