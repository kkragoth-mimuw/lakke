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
    env <- evalStmt x
    local (const env) (evalStmts xs)

evalStmt :: Stmt -> Eval Env

evalStmt Empty = ask
evalStmt (BStmt (Block stmts)) = evalStmts stmts >> ask
evalStmt (DeclS decl) = evalDecl decl
evalStmt (PrintLn expr) = do
    e <- evalExpr expr

    case isSimpleType e of
        True  -> tell [simpleTypeToString e]
        False -> throwError $ RErrorInvalidTypeNoInfo
    ask

evalStmt (Cond expr block) = evalStmt (CondElse expr block (Block []))
evalStmt (CondElse expr (Block blockTrue) (Block blockFalse)) = do
    e <- evalExpr expr
    case e of
        LKBool True  -> evalStmts blockTrue
        LKBool False -> evalStmts blockFalse
        _            -> throwError $ RErrorInvalidType Int "" expr
    ask

evalStmt (Incr id) = do
    ident <- evalId id

    variable <- extractVariable ident

    store <- get

    loc <- extractVariableLocation ident

    case variable of
        (LKInt i) -> put $ store & (vars . at loc ?~ LKInt (i + 1))
        _         -> throwError RErrorInvalidTypeNoInfo

    ask

evalStmt (Decr id) = do
    ident <- evalId id

    variable <- extractVariable ident

    store <- get

    loc <- extractVariableLocation ident

    case variable of
        (LKInt i) -> put $ store & (vars . at loc ?~ LKInt (i - 1))
        _         -> throwError RErrorInvalidTypeNoInfo

    ask

evalStmt (While expr block) = evalStmt (For Empty expr Empty block)

evalStmt (For initStmt expr outerStmt block@(Block stmts)) = do
    env <- evalStmt initStmt
    local (const env) (
        do
            condition <- evalExpr expr
            case condition of
                (LKBool False) -> ask
                (LKBool True) -> (do
                     evalStmts stmts;
                     evalStmt outerStmt;
                     evalStmt (For Empty expr outerStmt block)
                    ) `catchError` (\case
                                        LKBreak -> ask
                                        LKContinue -> do
                                                        evalStmt outerStmt
                                                        evalStmt (For Empty expr outerStmt block)
                                        error -> throwError error
                                    )
                _ -> throwError RErrorInvalidTypeNoInfo
        )

    ask

evalStmt Break = throwError LKBreak
evalStmt Continue = throwError LKContinue
evalStmt VRet = throwError $ LKReturn Nothing
evalStmt (Ret expr) = evalExpr expr >>= \value -> throwError $ LKReturn (Just value)
evalStmt (SExp expr) = evalExpr expr >> ask
evalStmt a = throwError $ REDebug $ show a
