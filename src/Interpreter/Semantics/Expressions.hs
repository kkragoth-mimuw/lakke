module Interpreter.Semantics.Expressions where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import Data.Map as Map

import AbsLakke
import Interpreter.EvalMonad
import Interpreter.Domains
import Interpreter.Values
import Interpreter.ErrorTypes

evalExpr :: Expr -> Eval LKValue
evalExpr (ELitInt n) = do
    return $ LKInt n

evalExpr (EAdd expr1 addop expr2) = do
    LKInt e1 <- evalExpr expr1
    LKInt e2 <- evalExpr expr2
    return $ LKInt $ e1 + e2

evalExpr (EVar id) = do
    env <- ask
    state <- get
    ident <- evalId id

    -- let venv = view varsEnv env
    -- let venv = (view varsEnv) env
    
    -- return $ LKInt 2

    case (env & (varsEnv & view)) ^.at ident of
        Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        Just loc -> case (state & (vars & view)) ^.at loc of
            Just var -> return var
            Nothing -> throwError RErrorMemoryLocation

    -- case (view (env . varsEnv)) ^.at ident of
        --   Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        --   Just loc -> return $ LKInt 1
          --case (view (state . vars)) ^.at loc of
           -- Just var -> return var
            --Nothing -> throwError RErrorMemoryLocation

evalId :: Id -> Eval Ident
evalId (Id n) = return n