module Interpreter.TypesUtils where

import           AbsLakke
import           Interpreter.Values

lkType :: LKValue -> Type
lkType lkValue = case lkValue of
    (LKInt _)    -> Int
    (LKString _) -> Str
    (LKBool _)   -> Bool
    _            -> Void

checkIfAllowedTypeOfValue :: [Type] -> LKValue -> Bool
checkIfAllowedTypeOfValue allowedTypes value = checkIfAllowedType allowedTypes (lkType value)

checkIfAllowedType :: [Type] -> Type -> Bool
checkIfAllowedType allowedTypes type' = type' `elem` allowedTypes

simpleTypes :: [Type]
simpleTypes = undefined

printValue :: LKValue -> String
printValue _ = undefined

isSimpleType :: LKValue -> Bool
isSimpleType lkValue = case lkValue of
    (LKInt _)    -> True
    (LKString _) -> True
    (LKBool _)   -> True

simpleTypeToString :: LKValue -> String
simpleTypeToString lkValue = case lkValue of
    (LKInt i)    -> show i
    (LKString s) -> show s
    (LKBool b)   -> show b
    _            -> ""
