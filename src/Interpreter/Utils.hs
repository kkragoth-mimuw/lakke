module Interpreter.Utils where

import           Control.Lens
import           Control.Monad.State
import           Data.Map                      as Map

import           AbsLakke
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains


getIdentFromArg :: Arg -> Ident
getIdentFromArg (VArg _ ident) = ident
getIdentFromArg (RArg _ ident) = ident


getTypeFromArg :: Arg -> Type
getTypeFromArg (VArg type_ _) = type_
getTypeFromArg (RArg type_ _) = type_


isVArg :: Arg -> Bool
isVArg (VArg _ _) = True
isVArg _          = False


mapRelOpToRelFunction :: (Ord a) => RelOp -> (a -> a -> Bool)
mapRelOpToRelFunction relOp = case relOp of
    LTH -> (<)
    LE  -> (<=)
    GTH -> (>)
    GE  -> (>=)
    EQU -> (==)
    NE  -> (/=)


mapMulOpToMulFunction :: MulOp -> (Integer -> Integer -> Integer)
mapMulOpToMulFunction mulOp = case mulOp of
    Mod   -> mod
    Times -> (*)
    Div   -> div


