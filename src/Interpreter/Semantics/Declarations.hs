{-# LANGUAGE ImplicitParams #-}

module Interpreter.Semantics.Declarations where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map                          as Map

import           AbsLakke
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions
import           Interpreter.DomainsUtils
import           Interpreter.Values

evalFuncDecl :: LKFunctionDef -> Eval Env
evalFuncDecl func@(LKFunctionDef fnType fnName args block) = do
  store <- get
  env <- ask

  i <- newloc funcDefs

  put (store & (funcDefs . at i ?~ func))
  return (env  & (funcsEnv . at fnName ?~ i))

isStmtDeclaration :: Stmt -> Bool
isStmtDeclaration stmt = case stmt of
    (DeclS _ ) -> True
    (ArrayDecl _ _ _ ) -> True
    (Struct _ ) -> True
    _ -> False

evalDeclDependencyInjection :: ([Stmt] -> Eval ()) -> Stmt -> Eval Env
evalDeclDependencyInjection evalStmts stmt = let ?evalStmts = evalStmts in evalDecl stmt

evalDecl :: (?evalStmts :: [Stmt] -> Eval ()) => Stmt -> Eval Env
evalDecl (DeclS (Decl type_ item)) = evalItem item
evalDecl (ArrayDecl arrayType expr ident) = undefined
evalDecl (Struct structDecl) = undefined
evalDecl _ = ask


evalItem :: (?evalStmts :: [Stmt] -> Eval ()) => Item -> Eval Env
evalItem (Init lvalue expr) = do
  value <- evalExpr expr
  name <- evalLValue lvalue

  store <- get

  i <- newloc vars

  put (store & (vars . at i ?~ value))

  env <- ask

  return (env & (varsEnv . at name ?~ i))
