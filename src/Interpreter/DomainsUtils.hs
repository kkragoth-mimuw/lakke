module Interpreter.DomainsUtils where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as Map
import           Data.Maybe

import           AbsLakke

import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad

import           Interpreter.Semantics.Domains


newloc :: Getting (Map.Map Location a) Store (Map.Map Location a) -> Eval Integer
newloc storeGetter = do
    store <- get
    return $ toInteger $ Map.size (store & (storeGetter & view))


storeNewVariable :: Ident -> LKValue -> Eval Env
storeNewVariable name value = do
    store <- get

    i <- newloc vars
  
    put (store & (vars . at i ?~ value))
  
    env <- ask
  
    return (env & (varsEnv . at name ?~ (i, getLevel env)))


extractVariable :: Ident -> Eval LKValue
extractVariable ident = extractVariableFromStore =<< extractVariableLocation ident


extractVariableLocation :: Ident -> Eval Location
extractVariableLocation ident = do
    env <- ask

    let maybeLocation = env ^. varsEnv . at ident

    when (isNothing maybeLocation)
        (throwError $ initRuntimeErrorNoLocation (RErrorUnknownIdentifier (show ident)))

    return $ fst $ fromJust maybeLocation


extractVariableFromStore :: Location -> Eval LKValue
extractVariableFromStore location = do
    store <- get

    let maybeValue = store ^. vars . at location

    when (isNothing maybeValue)
        (throwError $ initRuntimeErrorNoLocation RErrorMemoryLocation)

    return $ fromJust maybeValue


assignVariable :: Ident -> LKValue -> Eval ()
assignVariable ident value = do
    store <- get
    loc <- extractVariableLocation ident

    put $ store & (vars . at loc ?~ value)


overIntegerVariable :: Ident -> (Integer -> Integer) -> Eval()
overIntegerVariable ident f = do

    variable <- extractVariable ident

    store <- get
    loc <- extractVariableLocation ident

    case variable of
        (LKInt i) -> assignVariable ident (LKInt (f i))
        _         -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo


copySimpleVariable :: LKValue -> Eval Location
copySimpleVariable value = do
    i <- newloc vars

    store <- get

    put $ store & (vars . at i ?~ value)

    return i


updateEnv :: (Location, Ident) -> Env -> Env
updateEnv (location, ident) env = env & (varsEnv . at ident ?~ (location, getLevel env))


getLevel :: Env -> EnvLevel
getLevel env = env ^. level


increaseLevel :: Env -> Env
increaseLevel env = env &  (level +~ 1)


checkIfIsAlreadyDeclaredAtCurrentLevel :: Ident -> Eval ()
checkIfIsAlreadyDeclaredAtCurrentLevel ident = do
    env <- ask

    let currentLevel = getLevel env

    let maybeLocation = env ^. varsEnv . at ident

    case maybeLocation of
        Just (_, levelDeclared) | levelDeclared >= currentLevel -> throwError $ initRuntimeErrorNoLocation (RERedeclaration ident)
        _ -> return ()
