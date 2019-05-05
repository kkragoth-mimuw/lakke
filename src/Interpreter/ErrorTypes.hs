module Interpreter.ErrorTypes where

import           Text.Printf

import           AbsLakke

import           Interpreter.Semantics.Domains

data RuntimeError = RErrorUnknownIdentifier String
                  | RErrorDivisonByZero
                  | RErrorMemoryLocation
                  | RErrorNoMainFunction
                  | RErrorInvalidTypeNoInfo
                  | RErrorInvalidType Type String Expr
                  | REValueIsNotPrintable
                  | REErrorCast
                  | RENoReturnValue
                  | RENotLValue
                  | RERedeclaration Ident
                  | REInvalidNumberOfArgumentsSupplied
                  | REDebug String
                  | LKBreak
                  | LKContinue
                  | LKReturn (Maybe LKValue)


instance Show RuntimeError where
    show RErrorNoMainFunction                     = "No main function defined"
    show (RErrorUnknownIdentifier identifier)     = "Unknown identifier " ++ identifier
    show RErrorMemoryLocation                     = "Location not in store"
    show RErrorDivisonByZero                      = "Division by zero"
    show REErrorCast                              = "Error in cast"
    show (RErrorInvalidType exprType reason expr) = printf "Invalid type: %s in %s. %s" (show exprType) (show expr) reason
    show RErrorInvalidTypeNoInfo                  = "Invalid type"
    show REValueIsNotPrintable                    = "Unable to print value"
    show (RERedeclaration (Ident ident))          = "Redeclaration of: " ++ ident
    show RENoReturnValue                          = "No return value"
    show (REDebug s)                              = s
    show RENotLValue                              = "Not LValue"
    show LKBreak                                  = "break"
    show LKContinue                               = "continue"
    show (LKReturn value)                         = "Invalid return value " ++ show value
