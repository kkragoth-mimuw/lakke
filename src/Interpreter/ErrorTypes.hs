module Interpreter.ErrorTypes where

data RuntimeError = RErrorUnknownIdentifier String
                  | RErrorDivisonByZero
                  | RErrorMemoryLocation
                    deriving (Show)