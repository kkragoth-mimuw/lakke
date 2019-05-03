{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Semantics.Domains where

import           Control.Lens
import qualified Data.Map           as Map
import qualified Data.Vector        as V

import           AbsLakke
import           Interpreter.Values

type Location = Integer

data Env   = Env   {    _varsEnv :: Map.Map Ident Location
                   ,   _funcsEnv :: Map.Map Ident Location
                   , _structsEnv :: Map.Map Ident Location
                   } deriving (Show)

data Store = Store {       _vars :: Map.Map Location LKValue
                   ,   _funcDefs :: Map.Map Location LKFunctionDef
                   , _structDefs :: Map.Map Location LKStructDef
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
