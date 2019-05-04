{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Semantics.Domains where

import           Control.Lens
import qualified Data.Map           as Map
import qualified Data.Vector        as V

import           AbsLakke
import           Interpreter.Values

type Location = Integer
type EnvLevel    = Integer

data Env   = Env   {    _varsEnv :: Map.Map Ident (Location, EnvLevel)
                   ,   _funcsEnv :: Map.Map Ident (Location, EnvLevel)
                   , _structsEnv :: Map.Map Ident (Location, EnvLevel)
                   ,      _level :: EnvLevel
                   } deriving (Show)

data Store = Store {       _vars :: Map.Map Location LKValue
                   ,   _funcDefs :: Map.Map Location LKFunctionDef
                   , _structDefs :: Map.Map Location LKStructDef
                   } deriving (Show)

initEnv   = Env   {    _varsEnv        = Map.empty
                  ,   _funcsEnv        = Map.empty
                  , _structsEnv        = Map.empty
                  ,      _level        = 0
                  }

initStore = Store {       _vars       = Map.empty
                  ,   _funcDefs       = Map.empty
                  , _structDefs       = Map.empty
                  }

makeLenses ''Env
makeLenses ''Store
