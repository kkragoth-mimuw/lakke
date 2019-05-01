module Interpreter.Domains where

import AbsLakke
import Interpreter.Values

import qualified Data.Map as Map

data Env   = Env   { vars         :: Map.Map Ident Integer
                   , funcs        :: Map.Map Ident Integer
                   , structs      :: Map.Map Ident Integer    
                   } deriving (Show)

data Store = Store { variables    :: Map.Map Integer LKValue
                   , functionDefs :: Map.Map Integer LKFunctionDef
                   , structDefs   :: Map.Map Integer LKStructDef
                   } deriving (Show)
                   
initEnv   = Env   { vars           = Map.empty
                  , funcs          = Map.empty
                  , structs        = Map.empty
                  }

initStore = Store { variables      = Map.empty
                  , functionDefs   = Map.empty
                  , structDefs     = Map.empty
                  }
