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
import           Interpreter.FuncUtils
import           Interpreter.DomainsUtils

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
runMainBlock env store = local (const env) ( do
    (LKFunction (LKFunctionDef argType _ args (Block stmts)) mainEnv) <- extractVariable (Ident "main")

    unless (null args) (throwError REMainHasArguments)

    local (const mainEnv) (evalStmts stmts) `catchError` catchReturnMain argType
  )