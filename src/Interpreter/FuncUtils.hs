{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase     #-}

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
        throwError $ initRuntimeErrorNoLocation RENoReturnValue


catchReturnValueBuilder :: Bool -> Type -> RuntimeErrorWithLogging -> Eval LKValue
catchReturnValueBuilder isMain returnType runtimeError =
    case runtimeError of
        RuntimeErrorWithLogging (LKReturn value) _ _ -> case value of
            Just returnValue | lkType returnValue == returnType -> return returnValue
            Nothing -> if isMain || returnType == Void then
                            return LKVoid
                        else
                            throwError $ initRuntimeErrorNoLocation RENoReturnValue
            _ -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo
        error -> throwError error


catchReturnMain :: Type -> RuntimeErrorWithLogging -> Eval ()
catchReturnMain returnType runtimeError  = catchReturnValueBuilder True returnType runtimeError >> return ()


catchReturn :: Type -> RuntimeErrorWithLogging -> Eval LKValue
catchReturn = catchReturnValueBuilder False


lambSuppliedArgToArg :: LambSuppliedArgWithType -> Arg
lambSuppliedArgToArg = \case LambSuppliedVArgWithType ident argType -> VArg argType ident
                             LambSuppliedRArgWithType ident argType -> RArg argType ident

catchNoMainIdentifier :: RuntimeErrorWithLogging -> Eval LKValue
catchNoMainIdentifier = \case RuntimeErrorWithLogging (RErrorUnknownIdentifier identifier) _ _ | identifier == show (Ident "main") -> throwError $ initRuntimeErrorNoLocation RErrorNoMainFunction
                              error -> throwError error