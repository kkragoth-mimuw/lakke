module Typechecker.EnvironmentUtils where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Maybe

import AbsLakke

import Typechecker.Environment
import Typechecker.TypecheckMonad
import Typechecker.Errors

extractVariableType :: Ident -> TCM Type
extractVariableType ident = do
    env <- ask

    let maybeLocation = env ^. tcmTypes . at ident

    when (isNothing maybeLocation)
        (throwError $ initTypecheckError $ TCUndeclaredVariable ident)

    return $ fst $ fromJust maybeLocation

checkIfIsAlreadyDeclaredAtCurrentLevel :: Ident -> TCM ()
checkIfIsAlreadyDeclaredAtCurrentLevel ident = do
    env <- ask

    let currentLevel = getLevel env

    let maybeLocation = env ^. tcmTypes . at ident

    case maybeLocation of
        Just (_, levelDeclared) | levelDeclared >= currentLevel -> throwError $ initTypecheckError (TCRedeclaration ident)
        _ -> return ()


getLevel :: TCMEnv -> Integer
getLevel env = env ^. tcmLevel

increaseLevel :: TCMEnv -> TCMEnv
increaseLevel env = env &  (tcmLevel +~ 1)

indicateLoop :: TCMEnv -> TCMEnv
indicateLoop env = env & (tcmIsInLoop .~ True)

isInLoop :: TCMEnv -> Bool
isInLoop env = env ^. tcmIsInLoop

indicateReturnType :: TCMEnv -> Type -> TCMEnv
indicateReturnType env type_ = env & (tcmFunctionReturnType ?~ type_)

getReturnType :: TCMEnv -> Maybe Type
getReturnType env = env ^. tcmFunctionReturnType

              