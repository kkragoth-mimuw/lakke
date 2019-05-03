module Interpreter.DomainsUtils where

import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Lens

import           AbsLakke

import           Interpreter.Semantics.Domains
import           Interpreter.EvalMonad
import           Interpreter.ErrorTypes
import           Interpreter.Values


extractVariable :: Ident -> Eval LKValue
extractVariable ident = extractVariableLocation ident >>= extractVariableFromStore


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