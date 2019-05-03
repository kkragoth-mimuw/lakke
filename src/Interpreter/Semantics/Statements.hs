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
import           Interpreter.Semantics.Declarations
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions
import           Interpreter.TypesUtils
import           Interpreter.Values


evalStmts :: [Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (x:xs) = do
    env <- evalStmtOrDeclaration x
    local (const env) (evalStmts xs)


evalStmtOrDeclaration :: Stmt -> Eval Env
evalStmtOrDeclaration stmt = case stmt of
    (DeclS decl) -> evalDecl decl
    (ArrayDecl arrayType expr ident) -> evalArrayDecl arrayType expr ident
    (Struct structDecl) -> evalStructDecl structDecl
    _ -> evalStmt stmt >> ask


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
    env <- evalStmtOrDeclaration initStmt
    local (const env) (
        do
            condition <- evalExpr expr
            case condition of
                (LKBool False) -> return ()
                (LKBool True) -> (
                    do
                                evalStmts stmts
                                evalStmt outerStmt
                                evalStmt (For Empty expr outerStmt block)
                    ) `catchError` (
                        \case
                                        LKBreak -> return ()
                                        LKContinue -> do
                                                    evalStmt outerStmt
                                                    evalStmt (For Empty expr outerStmt block)
                                        error -> throwError error
                                    )
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

evalStmt (SExp expr) = void $ evalExpr expr

evalStmt (BStmt (Block stmts)) = evalStmts stmts

evalStmt VRet = throwError $ LKReturn Nothing

evalStmt (Print expr) = evalExpr expr >>= tellValue

evalStmt (Ret expr) = evalExpr expr >>= \value -> throwError $ LKReturn (Just value)

evalStmt (Incr lvalue) = evalLValue lvalue >>= \ident -> overIntegerVariable ident (1 +)

evalStmt (Decr lvalue) = evalLValue lvalue >>= \ident -> overIntegerVariable ident (1 -)
