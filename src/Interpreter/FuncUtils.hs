{-# LANGUAGE ImplicitParams #-}

module Interpreter.FuncUtils where

import           Control.Monad.Except
import           Debug.Trace

import           AbsLakke

import           Interpreter.EvalMonad
import           Interpreter.DomainsUtils
import           Interpreter.Semantics.Domains
import           Interpreter.ErrorTypes
import           Interpreter.TypesUtils


checkIfFunctionShouldReturnSomething :: Type -> Eval LKValue
checkIfFunctionShouldReturnSomething returnType =
    if returnType == Void then
        return LKVoid
    else
        throwError RENoReturnValue


catchReturnValueBuilder :: Bool -> Type -> RuntimeError -> Eval LKValue
catchReturnValueBuilder isMain returnType runtimeError = do
    traceM $ "catch return"
    case runtimeError of
        LKReturn value -> case value of
            Just returnValue | lkType returnValue == returnType -> return returnValue
            Nothing -> if isMain || returnType == Void then
                            return LKVoid
                        else
                            throwError RENoReturnValue
            _ -> throwError RErrorInvalidTypeNoInfo
        error -> throwError error

catchReturnMain :: Type -> RuntimeError -> Eval ()
catchReturnMain returnType runtimeError  = catchReturnValueBuilder True returnType runtimeError >> return ()

catchReturn :: Type -> RuntimeError -> Eval LKValue
catchReturn = catchReturnValueBuilder False