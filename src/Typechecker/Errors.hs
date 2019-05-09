module Typechecker.Errors where

import           Data.Char
import           Text.Printf
import           System.Console.Pretty         (Color (..), Style (..), bgColor, color, style, supportsPretty)

import           AbsLakke
import           PrintLakke

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
    show (TCInvalidTypeExpectedType type' allowedType) = printf "Invalid type: %s. Expected: %s" (show type') (show allowedType)
    show (TCInvalidTypeExpectedTypes type' allowedTypes) = printf "Invalid type: $s. Expected %s" (show type') (show $ map show allowedTypes)
    show (TCRedeclaration ident)                        = printf "Tried to redeclare: %s" (show ident)
    show TCBreak                                       = printf "Break stmt not in loop"
    show TCContinue                                    = printf "Continue stmt not in loop"
    show TCNotLValue                                   = "Incorrect lvalue"
    show (TCDebug str)                                 = str
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
    mapM_ (\line -> putStrLn ("in: " ++  trim line))  stack    


trimLeft :: String -> String
trimLeft = dropWhile isSpace
    
trimRight :: String -> String
trimRight str | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs
    
trim :: String -> String
trim = trimLeft . trimRight

