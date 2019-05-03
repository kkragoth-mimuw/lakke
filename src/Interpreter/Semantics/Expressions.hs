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
import           Interpreter.Utils

evalExpr :: Expr -> Eval LKValue

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


evalExpr rel@(ERel exprLeft relOp exprRight) = do
    eLeft <- evalExpr exprLeft
    eRight <- evalExpr exprRight

    case (eLeft, eRight) of
        (LKInt l, LKInt r) -> return $ LKBool ((mapRelOpToRelFunction relOp) l r)
        _ -> throwError $ RErrorInvalidType Int "" rel

evalExpr mul@(EMul exprLeft mulOp exprRight) = do
    eLeft <- evalExpr exprLeft
    eRight <- evalExpr exprRight
    case (eLeft, eRight) of
        (LKInt l, LKInt r) -> return $ LKInt ((mapMulOpToMulFunction mulOp) l r)
        _ -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EString str) = return $ LKString str
evalExpr (ELitInt int) = return $ LKInt int
evalExpr ELitTrue  = return $ LKBool True
evalExpr ELitFalse = return $ LKBool False
evalExpr a = throwError $ REDebug $ show a
evalId :: Id -> Eval Ident
evalId (Id n) = return n

