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
import           Interpreter.Semantics.Expressions
import           Interpreter.Values


newloc :: Getting (Map.Map Location a) Store (Map.Map Location a) -> Eval Integer
newloc storeGetter = do
    store <- get
    return $ toInteger $ Map.size (store & (storeGetter & view))


extractVariable :: Ident -> Eval LKValue
extractVariable ident = extractVariableFromStore =<< extractVariableLocation ident


extractVariableLocation :: Ident -> Eval Location
extractVariableLocation ident = do
    env <- ask

    let maybeLocation = env ^. varsEnv .at ident

    when (isNothing maybeLocation)
        (throwError $ RErrorUnknownIdentifier (show ident))

    return $ fromJust maybeLocation


extractVariableFromStore :: Location -> Eval LKValue
extractVariableFromStore location = do
    store <- get

    let maybeValue = store ^. vars .at location

    when (isNothing maybeValue)
        (throwError RErrorMemoryLocation)

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
        _         -> throwError RErrorInvalidTypeNoInfo
