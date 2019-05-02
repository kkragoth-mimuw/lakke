module Interpreter.ErrorTypes where

data RuntimeError = RErrorUnknownIdentifier String
                  | RErrorDivisonByZero
                  | RErrorMemoryLocation
                  | RErrorNoMainFunction

instance Show RuntimeError where
    show RErrorNoMainFunction = "No main function defined"