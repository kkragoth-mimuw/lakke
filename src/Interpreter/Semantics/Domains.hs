{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Semantics.Domains where

import           Control.Lens
import qualified Data.Map           as Map
import qualified Data.Vector        as V

import           AbsLakke

type Location = Integer
type EnvLevel    = Integer

data Env   = Env   {    _varsEnv :: Map.Map Ident (Location, EnvLevel)
                   , _structsEnv :: Map.Map Ident (Location, EnvLevel)
                   ,      _level :: EnvLevel
                   } deriving (Show)

data Store = Store {       _vars :: Map.Map Location LKValue
                   , _structDefs :: Map.Map Location LKStructDef
                   } deriving (Show)

initEnv   = Env   {    _varsEnv        = Map.empty
                  , _structsEnv        = Map.empty
                  ,      _level        = 0
                  }

initStore = Store {       _vars       = Map.empty
                  , _structDefs       = Map.empty
                  }

data LKValue  = LKInt Integer
              | LKBool Bool
              | LKString String
              | LKFunction LKFunctionDef Env
              | LKStruct LKStructDef (Map.Map Ident LKValue)
              | LKNotInitFunction
              | LKVoid
              deriving (Show)


data LKFunctionDef = LKFunctionDef Type Ident [Arg] Block
                    deriving (Show)


newtype LKStructDef = LKStructDef (Map.Map Ident Type)
                deriving (Show)


makeLenses ''Env
makeLenses ''Store
