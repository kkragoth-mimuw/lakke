module Interpreter.Semantics.Statements where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           AbsLakke

import           Debug.Trace
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Declarations
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions
import           Interpreter.Types
import           Interpreter.Values

evalStmts :: [Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (x:xs) = do
    env <- evalStmt x
    local (const env) (evalStmts xs)

evalStmt :: Stmt -> Eval Env

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

    store <- get
    env <- ask

    case env ^. varsEnv .at ident of
        Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        Just loc -> case store ^. vars .at loc of
            Nothing  -> throwError RErrorMemoryLocation
            Just var -> case var of
                (LKInt i) -> put $ store & vars . at loc ?~ LKInt (i + 1)
                _         -> throwError RErrorInvalidTypeNoInfo
                
    ask

evalStmt whileStmt@(While expr (Block stmts)) = do
    e <- evalExpr expr

    case e of
        (LKBool True) -> do
                         evalStmts stmts
                         evalStmt whileStmt
        (LKBool False) -> ask
        _ -> throwError RErrorInvalidTypeNoInfo

evalStmt a = throwError $ REDebug $ show a
