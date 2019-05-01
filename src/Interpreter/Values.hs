module Interpreter.Values where

import Data.Map as Map

import AbsLakke

data LKValue  = LKInt Integer
              | LKBool Bool
              | LKString String
              | LKFunc LKFunctionDef
              | LKStruct LKStructDef (Map.Map Ident LKValue)
                deriving (Show)

                
data FuncArgument = FuncArgument { ident   :: Ident
                                 , byValue :: Bool
                                 , argType :: Type
                                 }
                     deriving (Show)


data LKFunctionDef = LKFunction Type [FuncArgument] Block
                     deriving (Show)


newtype LKStructDef = LKStructDef (Map.Map Ident Type)
                   deriving (Show)
