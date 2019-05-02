module Interpreter.ErrorTypes where

data RuntimeError = RErrorUnknownIdentifier String
                  | RErrorDivisonByZero
                  | RErrorMemoryLocation
                  | RErrorNoMainFunction

instance Show RuntimeError where
    show RErrorNoMainFunction                 = "No main function defined"
    show (RErrorUnknownIdentifier identifier) = "Unknown identifier " ++ identifier
    show RErrorMemoryLocation                 = "Location not in store"
