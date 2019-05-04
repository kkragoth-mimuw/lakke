module Interpreter.Program where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           AbsLakke
import           Interpreter.Debug
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Values

import           Interpreter.Semantics.Domains
import           Interpreter.Semantics.Statements
import           Interpreter.Semantics.TopDefs

runProgram :: (Executable program) => Env -> Store -> program -> ((Either RuntimeError (), Store), [String])
runProgram env st program = runWriter (runStateT (runExceptT (runReaderT (exec program) env)) st)

class Executable f where
  exec :: f -> Eval ()

instance Executable Program where
  exec (Program topdefs) = do
    env <- evalTopDefs topdefs
    store <- get

    runMainBlock env store

runMainBlock :: Env -> Store -> Eval ()
runMainBlock env store =
  case (env & (funcsEnv & view)) ^.at (Ident "main") of
    Nothing -> throwError $ RErrorNoMainFunction
    Just (loc, 0) -> case (store & (funcDefs & view)) ^.at loc of
      Nothing                                  -> throwError RErrorNoMainFunction
      Just (LKFunctionDef _ _ _ (Block stmts)) -> local (const env) (evalStmts stmts)
    Just (loc, _) -> throwError $ RErrorNoMainFunction
