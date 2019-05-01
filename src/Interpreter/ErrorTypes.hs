module Interpreter.ErrorTypes where

data RuntimeError = RErrorUnknownIdentifier String
                  | RErrorDivisonByZero
                    deriving (Show)