module Typechecker.Errors where

import           AbsLakke
import           PrintLakke

defaultLevelOfLogging = 5

data TypecheckError = TCInvalidType Type
                    | TCInvalidTypeExpectedType Type Type
                    | TCInvalidTypeExpectedTypes Type [ Type ]
                    | TCInvalidFunctionAppTypes [Type] [Type]
                    | TCMNotLValue
                    | TCMRedeclaration Ident
                    | TCBreak
                    | TCContinue
                    | TCNotLValue
                    | TCInvalidNumberOfArguments

instance Show TypecheckError where
    show a = ""

data TypecheckErrorWithLogging = TypecheckErrorWithLogging TypecheckError Integer [String] deriving (Show)

initTypecheckError :: TypecheckError -> TypecheckErrorWithLogging
initTypecheckError error = TypecheckErrorWithLogging error defaultLevelOfLogging []

appendLogToTypecheckError :: (Print a) => TypecheckErrorWithLogging -> a -> TypecheckErrorWithLogging
appendLogToTypecheckError typecheckErrorWithLogging@(TypecheckErrorWithLogging error loggingLevel msg) location | loggingLevel <= 0 = typecheckErrorWithLogging
appendLogToTypecheckError                           (TypecheckErrorWithLogging error loggingLevel msg) location = TypecheckErrorWithLogging error (loggingLevel - 1) (msg ++ [printTree location])
