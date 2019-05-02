module Interpreter.ErrorTypes where

import           Text.Printf

import           AbsLakke

data RuntimeError = RErrorUnknownIdentifier String
                  | RErrorDivisonByZero
                  | RErrorMemoryLocation
                  | RErrorNoMainFunction
                  | RErrorInvalidTypeNoInfo
                  | RErrorInvalidType Type String Expr
                  | REDebug String

instance Show RuntimeError where
    show RErrorNoMainFunction                     = "No main function defined"
    show (RErrorUnknownIdentifier identifier)     = "Unknown identifier " ++ identifier
    show RErrorMemoryLocation                     = "Location not in store"
    show (RErrorInvalidType exprType reason expr) = printf "Invalid type: %s in %s. %s" (show exprType) (show expr) reason
    show RErrorInvalidTypeNoInfo                  = "Invalid type"
    show (REDebug s)                              = s