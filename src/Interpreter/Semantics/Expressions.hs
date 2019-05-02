module Interpreter.Semantics.Expressions where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                      as Map

import           AbsLakke
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.Values

evalExpr :: Expr -> Eval LKValue
evalExpr (ELitInt n) = do
    return $ LKInt n

evalExpr (EAdd expr1 addop expr2) = do
    LKInt e1 <- evalExpr expr1
    LKInt e2 <- evalExpr expr2
    return $ LKInt $ e1 + e2

evalExpr (EVar id) = do
    env <- ask
    store <- get
    ident <- evalId id

    case (env & (varsEnv & view)) ^.at ident of
        Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        Just loc -> case (store & (vars & view)) ^.at loc of
            Just var -> return var
            Nothing  -> throwError RErrorMemoryLocation

evalId :: Id -> Eval Ident
evalId (Id n) = return n
