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
import           Interpreter.Utils
import           Interpreter.Values

evalFuncDecl :: LKFunctionDef -> Eval Env
evalFuncDecl func@(LKFunctionDef fnType fnName args block) = do
  state <- get
  env <- ask

  i <- newloc funcDefs
  put (state & (funcDefs . at i ?~ func))
  return (env  & (funcsEnv . at fnName ?~ i))

evalDecl :: Decl -> Eval Env
evalDecl (Decl type_ item) = evalItem item

evalItem :: Item -> Eval Env
evalItem (Init id expr) = do
  value <- evalExpr expr
  name <- evalId id

  state <- get

  let i = toInteger $ Map.size (state & (vars & view))

  tell $ [show i]

  put (state & (vars . at i ?~ value))

  env <- ask

  return (env & (varsEnv . at name ?~ i))
