module Typechecker.TypecheckMonad where

import Control.Monad.Except
import Control.Monad.Reader

import Typechecker.Errors
import Typechecker.Environment

type TCM a = (ExceptT TypecheckErrorWithLogging (Reader TCMEnv)) a