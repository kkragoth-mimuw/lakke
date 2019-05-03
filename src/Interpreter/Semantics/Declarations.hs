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

evalDecl :: Decl -> Eval Env
evalDecl (Decl type_ item) = evalItem item

evalArrayDecl :: Type -> Expr -> Ident -> Eval Env
evalArrayDecl arrayType expr ident = undefined

evalStructDecl :: StructDecl -> Eval Env
evalStructDecl structDecl = undefined
evalItem :: Item -> Eval Env
evalItem (Init lvalue expr) = do
  value <- evalExpr expr
  name <- evalLValue lvalue

  store <- get

  i <- newloc vars

  put (store & (vars . at i ?~ value))

  env <- ask

  return (env & (varsEnv . at name ?~ i))
