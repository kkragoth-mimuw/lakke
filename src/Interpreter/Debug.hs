module Interpreter.Debug where

import           Debug.Pretty.Simple           (pTraceShowM)
import           Debug.Trace
import           System.Console.Pretty         (Color (..), Style (..), bgColor, color, style, supportsPretty)

import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains

debug :: Env -> Store -> Eval ()
debug env store = do
    traceM $ color Yellow "Debug"
    pTraceShowM store
    pTraceShowM env

