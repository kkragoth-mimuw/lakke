module Interpreter.Utils where

import           Control.Lens
import           Control.Monad.State
import           Data.Map                      as Map

import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains


newloc :: Getting (Map.Map Integer a) Store (Map.Map Integer a) -> Eval Integer
newloc storeGetter = do
    store <- get
    return $ toInteger $ Map.size (store & (storeGetter & view))
