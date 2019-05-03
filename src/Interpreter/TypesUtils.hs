module Interpreter.TypesUtils where

import Interpreter.Values

-- checkIfAllowedType 

isSimpleType :: LKValue -> Bool
isSimpleType lkValue = case lkValue of
    (LKInt _) -> True
    (LKString _) -> True
    (LKBool _) -> True

simpleTypeToString :: LKValue -> String
simpleTypeToString lkValue = case lkValue of
    (LKInt i) -> show i
    (LKString s) -> show s
    (LKBool b) -> show b
    _ -> ""
