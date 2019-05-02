module Interpreter.Utils where

import           Control.Lens
import           Control.Monad.State
import           Data.Map                      as Map

import AbsLakke
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains


newloc :: Getting (Map.Map Integer a) Store (Map.Map Integer a) -> Eval Integer
newloc storeGetter = do
    store <- get
    return $ toInteger $ Map.size (store & (storeGetter & view))

mapRelOpToRelFunction :: (Ord a) => RelOp -> (a -> a -> Bool)
mapRelOpToRelFunction relOp = case relOp of
    LTH -> (<)
    LE -> (<=)
    GTH -> (>)
    GE -> (>=)
    EQU -> (==)
    NE -> (/=)

mapMulOpToMulFunction :: MulOp -> (Integer -> Integer -> Integer)
mapMulOpToMulFunction mulOp = case mulOp of
    Mod -> mod
    Times -> (*)
    Div -> div