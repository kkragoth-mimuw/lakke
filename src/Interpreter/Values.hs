module Interpreter.Values where

import           Data.Map as Map

import           AbsLakke

data LKValue  = LKInt Integer
              | LKBool Bool
              | LKString String
              | LKFunction LKFunctionDef
              | LKStruct LKStructDef (Map.Map Ident LKValue)
                deriving (Show)


data LKFunctionDef = LKFunctionDef Type Ident [Arg] Block
                     deriving (Show)


newtype LKStructDef = LKStructDef (Map.Map Ident Type)
                   deriving (Show)
