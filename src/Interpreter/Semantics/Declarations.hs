{-# LANGUAGE ImplicitParams #-}

module Interpreter.Semantics.Declarations where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Debug.Trace
import           Data.Map                          as Map

import           AbsLakke
import           Interpreter.EvalMonad
import           Interpreter.ErrorTypes
import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Expressions
import           Interpreter.DomainsUtils
import           Interpreter.TypesUtils


isStmtDeclaration :: Stmt -> Bool
isStmtDeclaration stmt = case stmt of
    (DeclS _ ) -> True
    (DeclF _ ) -> True
    _ -> False

evalDeclDependencyInjection :: ([Stmt] -> Eval ()) -> Stmt -> Eval Env
evalDeclDependencyInjection evalStmts stmt = let ?evalStmts = evalStmts in evalDeclWithErrorLogging stmt

evalDeclWithErrorLogging :: (?evalStmts :: [Stmt] -> Eval ()) => Stmt -> Eval Env
evalDeclWithErrorLogging stmt = evalDecl stmt `catchError` (\runtimeError -> throwError (appendLogToRuntimeError runtimeError stmt))

evalDecl :: (?evalStmts :: [Stmt] -> Eval ()) => Stmt -> Eval Env
evalDecl decl@(DeclS (Decl type_ (Init lvalue expr))) = do
  value <- evalExpr expr
  name <- evalLValue lvalue

  checkIfIsAlreadyDeclaredAtCurrentLevel name

  when (type_ /= lkType value) 
    (throwError $ initRuntimeError RErrorInvalidTypeNoInfo decl)

  storeNewVariable name value


evalDecl decl@(DeclS (Decl type_ (NoInit lvalue))) = do
  let value = defaultValue  type_
  name <- evalLValue lvalue

  checkIfIsAlreadyDeclaredAtCurrentLevel name

  storeNewVariable name value


evalDecl (DeclF (FNDef fnType fnName args block)) = do
  store <- get
  env <- ask

  checkIfIsAlreadyDeclaredAtCurrentLevel fnName

  i <- newloc vars

  let newEnv = (env  & (varsEnv . at fnName ?~ (i, getLevel env)))

  let func = LKFunction (LKFunctionDef fnType fnName args block) newEnv

  put (store & (vars . at i ?~ func))

  return newEnv

evalDecl _ = ask
