{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Semantics.Domains where

import           Control.Lens
import qualified Data.Map           as Map
import qualified Data.Vector        as V

import           AbsLakke
import           Interpreter.Values

data Env   = Env   {    _varsEnv :: Map.Map Ident Integer
                   ,   _funcsEnv :: Map.Map Ident Integer
                   , _structsEnv :: Map.Map Ident Integer
                   } deriving (Show)

data Store = Store {       _vars :: Map.Map Integer LKValue
                   ,   _funcDefs :: Map.Map Integer LKFunctionDef
                   , _structDefs :: Map.Map Integer LKStructDef
                   } deriving (Show)

initEnv   = Env   {    _varsEnv        = Map.empty
                  ,   _funcsEnv        = Map.empty
                  , _structsEnv        = Map.empty
                  }

initStore = Store {       _vars       = Map.empty
                  ,   _funcDefs       = Map.empty
                  , _structDefs       = Map.empty
                  }

makeLenses ''Env
makeLenses ''Store
