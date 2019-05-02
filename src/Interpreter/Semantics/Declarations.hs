module Interpreter.Semantics.Declarations where

import Data.Map as Map
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import AbsLakke
import Interpreter.EvalMonad
import Interpreter.Domains
import Interpreter.Semantics.Expressions


  
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