module Interpreter.TypesUtils where

import           Control.Monad.Except
import           Control.Monad.Writer
import           Debug.Trace

import           AbsLakke

import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.ErrorTypes


lkType :: LKValue -> Type
lkType lkValue =
    case lkValue of
        (LKInt _)    -> Int
        (LKString _) -> Str
        (LKBool _)   -> Bool
        (LKFunction (LKFunctionDef type' _ args _) _) ->  LambdaType (Prelude.map argToLambArg args)  type'
        LKVoid     -> Void
        _            -> Void


defaultValue :: Type -> LKValue
defaultValue type_ = case type_ of
    Int -> LKInt 0
    Bool -> LKBool False
    Str -> LKString ""
    LambdaType _ _ -> LKNotInitFunction

checkIfAllowedTypeOfValue :: [Type] -> LKValue -> Bool
checkIfAllowedTypeOfValue allowedTypes lkValue = checkIfAllowedType allowedTypes (lkType lkValue)


checkIfAllowedType :: [Type] -> Type -> Bool
checkIfAllowedType allowedTypes type' = type' `elem` allowedTypes


simpleTypes :: [Type]
simpleTypes = [Int, Str, Bool]


printValue :: LKValue -> String
printValue _ = undefined


isSimpleType :: LKValue -> Bool
isSimpleType lkValue = checkIfAllowedType simpleTypes (lkType lkValue)


simpleTypeToString :: LKValue -> String
simpleTypeToString lkValue = case lkValue of
    (LKInt i)    -> show i
    (LKString s) -> show s
    (LKBool b)   -> show b
    _            -> ""


tellValue :: LKValue -> Eval ()
tellValue lkValue =
    if isSimpleType lkValue then
        tell [simpleTypeToString lkValue]
    else
        throwError $ initRuntimeErrorNoLocation REValueIsNotPrintable


argToLambArg :: Arg -> LambArg
argToLambArg arg = case arg of 
    VArg type' _ -> LambVArg type'
    RArg type' _ -> LambRArg type'
        