{-# LANGUAGE LambdaCase #-}

module Interpreter.Semantics.Statements where

import           Control.Lens                       hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe
import           Debug.Trace

import           AbsLakke

import           Interpreter.Debug
import           Interpreter.DomainsUtils
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Declarations as LKDecl hiding (evalDecl, evalExpr)
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions  as LKExpr hiding (evalExpr)
import           Interpreter.TypesUtils


evalStmts :: [Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (x:xs) = do
    env <- evalStmtOrDeclaration x
    local (const env) (evalStmts xs)


evalStmtOrDeclaration :: Stmt -> Eval Env
evalStmtOrDeclaration stmt =
    if LKDecl.isStmtDeclaration stmt then
        evalDecl stmt
    else
        evalStmtWithErrorLogging stmt >> ask

evalStmtWithErrorLogging :: Stmt -> Eval ()
evalStmtWithErrorLogging stmt = evalStmt stmt `catchError` (\runtimeError -> throwError (appendLogToRuntimeError runtimeError stmt))

evalStmt :: Stmt -> Eval ()
evalStmt (Cond expr block) = evalStmt  (CondElse expr block (Block []))

evalStmt (CondElse expr (Block blockTrue) (Block blockFalse)) = do
    e <- evalExpr expr
    case e of
        LKBool True  -> evalStmts blockTrue
        LKBool False -> evalStmts blockFalse
        _            -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo


evalStmt (While expr block) = evalStmt (For Empty expr Empty block)

evalStmt (For initStmt expr outerStmt block@(Block stmts)) = do
    let evalForLoopStep       = evalStmts stmts >> evalStmtWithErrorLogging  outerStmt
    let recursiveEvalLoopBody = evalStmtWithErrorLogging  (For Empty expr outerStmt block)

    env <- evalStmtOrDeclaration initStmt
    local (const env) (
        do
            condition <- evalExpr expr
            case condition of
                (LKBool False) -> return ()
                (LKBool True) -> (evalForLoopStep >> recursiveEvalLoopBody)
                    `catchError` \case
                                    RuntimeErrorWithLogging LKContinue _ _ -> evalStmtWithErrorLogging  outerStmt >> recursiveEvalLoopBody
                                    RuntimeErrorWithLogging LKBreak _ _ -> return ()
                                    error -> throwError error
                _ -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo
        )


evalStmt (Ass lvalue expr) = do
    ident <- evalLValue lvalue
    variable <- extractVariable ident
    rvalue <- evalExpr expr

    if lkType variable == lkType rvalue then
        assignVariable ident rvalue
    else
        throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo


evalStmt Empty = return ()

evalStmt Break = throwError $ initRuntimeErrorNoLocation LKBreak

evalStmt Continue = throwError $ initRuntimeErrorNoLocation LKContinue

evalStmt (SExp expr) = evalExpr expr >> return ()

evalStmt (BStmt (Block stmts)) = local increaseLevel (evalStmts stmts)

evalStmt VRet = throwError $ initRuntimeErrorNoLocation (LKReturn Nothing)

evalStmt (Print expr) = evalExpr expr >>= tellValue

evalStmt (Ret expr) = evalExpr expr >>= \value -> throwError $ initRuntimeErrorNoLocation (LKReturn (Just value))

evalStmt (Incr lvalue) = evalLValue lvalue >>= \ident -> overIntegerVariable ident (1 +)

evalStmt (Decr lvalue) = evalLValue lvalue >>= \ident -> overIntegerVariable ident (1 -)

evalExpr :: Expr -> Eval LKValue
evalExpr = LKExpr.evalExprDependencyInjection evalStmts

evalDecl :: Stmt -> Eval Env
evalDecl = LKDecl.evalDeclDependencyInjection evalStmts
