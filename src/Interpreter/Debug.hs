module Interpreter.Debug where

-- import           Debug.Pretty.Simple           (pTraceShowM)
import           Debug.Trace

import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains

debug :: Env -> Store -> Eval ()
debug env store = do
    traceM "Debug"
    traceM $ show store
    traceM $ show env
    -- pTraceShowM store
    -- pTraceShowM env

