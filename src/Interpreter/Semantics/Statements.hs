{-# LANGUAGE LambdaCase #-}

module Interpreter.Semantics.Statements where

import           Control.Lens                       hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe

import           AbsLakke

import           Interpreter.Debug
import           Interpreter.DomainsUtils
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Declarations as LKDecl hiding (evalDecl, evalExpr)
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions  as LKExpr hiding (evalExpr)
import           Interpreter.TypesUtils
import           Interpreter.Values


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
        evalStmt stmt >> ask


evalStmt :: Stmt -> Eval ()
evalStmt (Cond expr block) = evalStmt (CondElse expr block (Block []))

evalStmt (CondElse expr (Block blockTrue) (Block blockFalse)) = do
    e <- evalExpr expr
    case e of
        LKBool True  -> evalStmts blockTrue
        LKBool False -> evalStmts blockFalse
        _            -> throwError $ RErrorInvalidType Int "" expr


evalStmt (While expr block) = evalStmt (For Empty expr Empty block)

evalStmt (For initStmt expr outerStmt block@(Block stmts)) = do
    let evalForLoopStep       = evalStmts stmts >> evalStmt outerStmt
    let recursiveEvalLoopBody = evalStmt (For Empty expr outerStmt block)

    env <- evalStmtOrDeclaration initStmt
    local (const env) (
        do
            condition <- evalExpr expr
            case condition of
                (LKBool False) -> return ()
                (LKBool True) -> (evalForLoopStep >> recursiveEvalLoopBody)
                    `catchError` \case
                                    LKContinue -> evalStmt outerStmt >> recursiveEvalLoopBody
                                    LKBreak -> return ()
                                    error -> throwError error
                _ -> throwError RErrorInvalidTypeNoInfo
        )


evalStmt (Ass lvalue expr) = do
    ident <- evalLValue lvalue
    variable <- extractVariable ident
    rvalue <- evalExpr expr

    if lkType variable == lkType rvalue then
        assignVariable ident rvalue
    else
        throwError RErrorInvalidTypeNoInfo


evalStmt Empty = return ()

evalStmt Break = throwError LKBreak

evalStmt Continue = throwError LKContinue

evalStmt (SExp expr) = evalExpr expr >> return ()

evalStmt (BStmt (Block stmts)) = local increaseLevel (evalStmts stmts)

evalStmt VRet = throwError $ LKReturn Nothing

evalStmt (Print expr) = evalExpr expr >>= tellValue

evalStmt (Ret expr) = evalExpr expr >>= \value -> throwError $ LKReturn (Just value)

evalStmt (Incr lvalue) = evalLValue lvalue >>= \ident -> overIntegerVariable ident (1 +)

evalStmt (Decr lvalue) = evalLValue lvalue >>= \ident -> overIntegerVariable ident (1 -)

evalExpr :: Expr -> Eval LKValue
evalExpr = LKExpr.evalExprDependencyInjection evalStmts

evalDecl :: Stmt -> Eval Env
evalDecl = LKDecl.evalDeclDependencyInjection evalStmts
