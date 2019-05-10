module Typechecker.Errors where

import           Data.List
import           System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
import           Text.Printf

import           AbsLakke
import           PrintLakke

import           Typechecker.Utils

defaultLevelOfLogging = 5

data TypecheckError = TCInvalidTypeExpectedType Type Type
                    | TCInvalidTypeExpectedTypes Type [ Type ]
                    | TCInvalidFunctionAppTypes [Type] [Type]
                    | TCUndeclaredVariable Ident
                    | TCMNotLValue
                    | TCRedeclaration Ident
                    | TCBreak
                    | TCContinue
                    | TCReturn
                    | TCNotLValue
                    | TCInvalidNumberOfArguments
                    | TCDebug String

instance Show TypecheckError where
    show (TCInvalidTypeExpectedType type' allowedType)   = printf "Invalid type: %s. Expected: %s" (prettyShowType type') (prettyShowType allowedType)
    show (TCInvalidTypeExpectedTypes type' allowedTypes) = printf "Invalid type: %s. Expected %s" (prettyShowType type') (intercalate " or " (map prettyShowType allowedTypes))
    show (TCRedeclaration (Ident ident))                 = printf "Tried to redeclare: %s" (show ident)
    show TCBreak                                         =  "Break stmt not in loop"
    show TCContinue                                      = "Continue stmt not in loop"
    show TCNotLValue                                     = "Incorrect lvalue"
    show TCInvalidNumberOfArguments                      = "Passed invalid number of arguments"
    show TCMNotLValue                                    = "Supplied expression is not lvalue"
    show (TCDebug str)                                   = "DEBUG:" ++ str
    show a = ""

data TypecheckErrorWithLogging = TypecheckErrorWithLogging TypecheckError Integer [String] deriving (Show)

initTypecheckError :: TypecheckError -> TypecheckErrorWithLogging
initTypecheckError error = TypecheckErrorWithLogging error defaultLevelOfLogging []

appendLogToTypecheckError :: (Print a) => TypecheckErrorWithLogging -> a -> TypecheckErrorWithLogging
appendLogToTypecheckError typecheckErrorWithLogging@(TypecheckErrorWithLogging error loggingLevel msg) location | loggingLevel <= 0 = typecheckErrorWithLogging
appendLogToTypecheckError                           (TypecheckErrorWithLogging error loggingLevel msg) location = TypecheckErrorWithLogging error (loggingLevel - 1) (msg ++ [printTree location])


pprintTypecheckerErrorMsg :: TypecheckErrorWithLogging -> IO ()
pprintTypecheckerErrorMsg wholeMsg@(TypecheckErrorWithLogging error _ stack) = do
    putStrLn (color Red ("Lakke's typechecker found a problem: " ++ show error))
    mapM_ (\line -> putStrLn ("\nFound in:\n " ++  trim line))  stack


prettyShowType :: Type -> String
prettyShowType type_ = case type_ of
    Int                            -> "int"
    Str                            -> "string"
    Bool                           -> "bool"
    Void                           -> "void"
    LambdaType lambArgs returnType -> printf "(%s) => %s" (intercalate "," (map (prettyShowType . lambArgToType) lambArgs)) (prettyShowType returnType)
