module Typechecker.Typecheck where

import Control.Monad.Except
import Control.Monad.Reader

import AbsLakke

import Typechecker.Errors
import Typechecker.TypecheckMonad
import Typechecker.Environment
import Typechecker.Statements

runTypecheck :: (Typecheckable program) => TCMEnv -> program -> (Either TypecheckErrorWithLogging ())
runTypecheck env program = runReader (runExceptT (typecheckProgram program)) env

class Typecheckable f where
    typecheckProgram :: f -> TCM ()

instance Typecheckable Program where
    typecheckProgram (Program topdefs) = typecheckTopDefs topdefs >> return ()
