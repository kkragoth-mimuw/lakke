module Typechecker.EnvironmentUtils where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens

import AbsLakke

import Typechecker.Environment
import Typechecker.TypecheckMonad
import Typechecker.Errors

checkIfIsAlreadyDeclaredAtCurrentLevel :: Ident -> TCM ()
checkIfIsAlreadyDeclaredAtCurrentLevel ident = do
    env <- ask

    let currentLevel = getLevel env

    let maybeLocation = env ^. tcmTypes . at ident

    case maybeLocation of
        Just (_, levelDeclared) | levelDeclared >= currentLevel -> throwError $ initTypecheckError (TCMRedeclaration ident)
        _ -> return ()


getLevel :: TCMEnv -> Integer
getLevel env = env ^. tcmLevel


increaseLevel :: TCMEnv -> TCMEnv
increaseLevel env = env &  (tcmLevel +~ 1)
              