{-# LANGUAGE TemplateHaskell #-}

module Typechecker.Environment where

import           Control.Lens
import qualified Data.Map     as Map

import           AbsLakke

data TCMEnv = TCMEnv { _tcmTypes          :: Map.Map Ident (Type, Integer)
               , _tcmLevel              :: Integer
               , _tcmIsInLoop           :: Bool
               , _tcmFunctionReturnType :: Maybe Type
               }

initTCMEnv = TCMEnv { _tcmTypes = Map.empty
              , _tcmLevel = 0
              , _tcmIsInLoop = False
              , _tcmFunctionReturnType = Nothing
              }

makeLenses ''TCMEnv
