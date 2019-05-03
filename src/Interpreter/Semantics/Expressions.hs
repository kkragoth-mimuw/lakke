{-# LANGUAGE LambdaCase #-}

module Interpreter.Semantics.Expressions where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                      as Map hiding (foldr)
import           Debug.Trace

import           AbsLakke

import           Interpreter.Debug
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.DomainsUtils
import           Interpreter.Semantics.Domains
import           Interpreter.Values
import           Interpreter.Utils

evalExprDependencyInjection :: [[Stmt] -> Eval ()] -> Expr -> Eval LKValue
evalExprDependencyInjection [evalStmts] (EApp lvalue args) = applyFunction evalStmts lvalue args
evalExprDependencyInjection _ expr = evalExpr expr

getIdentFromArg :: Arg -> Ident
getIdentFromArg (VArg _ ident) = ident
getIdentFromArg (RArg _ ident) = ident

updateEnv :: (Location, Ident) -> Env -> Env
updateEnv (location, ident) env = env & (varsEnv . at ident ?~ location)

applyFunction :: ([Stmt] -> Eval ()) -> LValue -> [Expr] -> Eval LKValue
applyFunction evalStmts lvalue exprs = do
    env <- ask
    store <- get
    ident <- evalLValue lvalue

    suppliedArgs <- mapM evalExpr exprs
    case (env & (funcsEnv & view)) ^.at ident of
        Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        Just loc -> case (store & (funcDefs & view)) ^.at loc of
            Just (LKFunctionDef returnType ident args (Block stmts)) -> do
                 newLocs <- Prelude.mapM copySimpleVariable suppliedArgs
                 let idents = Prelude.map getIdentFromArg args

                 let newEnv = foldr updateEnv env (zip newLocs idents)

                --  traceM $ show newLocs

                 store <- get

                --  debug newEnv store

                 (do local (const newEnv) (evalStmts stmts)
                     if returnType == Void then
                        return LKVoid
                     else
                        throwError RENoReturnValue
                  )
                 `catchError` (
                     \case
                        LKReturn value -> case value of
                                            Just returnValue -> return returnValue
                                            Nothing -> if returnType == Void then
                                                            return LKVoid
                                                       else
                                                            throwError RENoReturnValue
                        error -> throwError error
                  )
            Nothing  -> throwError RErrorMemoryLocation



evalExpr :: Expr -> Eval LKValue

evalExpr (EAdd expr1 addop expr2) = do
    LKInt e1 <- evalExpr expr1
    LKInt e2 <- evalExpr expr2
    case addop of
        Plus -> return $ LKInt $ e1 + e2
        Minus -> return $ LKInt $ e1 - e2
    
evalExpr (EVar lvalue) = do
    env <- ask
    store <- get
    ident <- evalLValue lvalue

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
evalExpr a = throwError $ REDebug $ "Expr exhausted " ++show a
evalLValue :: LValue -> Eval Ident
evalLValue (LValue n) = return n

