{-# LANGUAGE ImplicitParams #-}

module Interpreter.Semantics.Expressions where

import           Control.Lens
import           Control.Monad                 (join)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                (bimap)
import           Data.List
import           Data.Map                      as Map hiding (foldr, partition)
import           Debug.Trace

import           AbsLakke
import           PrintLakke

import           Interpreter.Debug
import           Interpreter.DomainsUtils
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.TypesUtils
import           Interpreter.Utils
import           Interpreter.FuncUtils


evalExprDependencyInjection :: ([Stmt] -> Eval ()) -> Expr -> Eval LKValue
evalExprDependencyInjection evalStmts expr = let ?evalStmts = evalStmts in evalExprWithErrorLogging expr

evalExprWithErrorLogging :: (?evalStmts :: [Stmt] -> Eval ()) => Expr -> Eval LKValue
evalExprWithErrorLogging expr = evalExpr expr `catchError` (\runtimeError -> throwError (appendLogToRuntimeError runtimeError expr))


evalExpr :: (?evalStmts :: [Stmt] -> Eval ()) => Expr -> Eval LKValue
evalExpr (EApp lvalue exprs) = do
    ident <- evalLValue lvalue
    func <- extractVariable ident
    evalFunctionApplication func exprs

evalExpr (ECast type_  expr) = do
    value <- evalExprWithErrorLogging expr

    unless (isSimpleType value && checkIfAllowedType simpleTypes type_)
        (throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo)

    case (type_, value) of
        (Str, LKInt value) -> return $ LKString (show value)
        _                  -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo


evalExpr (EAdd expr1 addop expr2) = do
    (l, r) <- evalExpr2 expr1 expr2

    case (l, r, addop) of
        (LKString l, LKString r, Plus) -> return $ LKString (l ++ r)
        (LKInt l, LKInt r, op)         -> return $ LKInt (mapAddOpToFunction op l  r)
        _                              -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo


evalExpr (EVar lvalue) = evalLValue lvalue >>= extractVariable

evalExpr (ERel exprLeft relOp exprRight) = do
    (left, right) <- evalExpr2 exprLeft exprRight

    case (left, right) of
        (LKString l, LKString r) -> return $ LKBool (mapRelOpToRelFunction relOp l r)
        (LKInt l, LKInt r)       -> return $ LKBool (mapRelOpToRelFunction relOp l r)
        _                        -> throwError $  initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo

evalExpr (EMul exprLeft mulOp exprRight) = do
    (left, right) <- evalExpr2 exprLeft exprRight
    case (left, right, mulOp) of
        (LKInt l, LKInt 0, Div ) -> throwError $ initRuntimeErrorNoLocation RErrorDivisonByZero
        (LKInt l, LKInt r, _)    -> return $ LKInt (mapMulOpToMulFunction mulOp l r)
        _                        -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo

evalExpr (Neg expr) = do
    var <- evalExprWithErrorLogging expr
    case var of
        (LKInt value) -> return $ LKInt (negate value)
        _             -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo

evalExpr (Not expr) = do
    cond <- evalExprWithErrorLogging expr
    case cond of
        (LKBool value) -> return $ LKBool (not value)
        _              -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo

evalExpr (EAnd expr1 expr2) = do
    (left, right) <- evalExpr2 expr1 expr2
    case (left, right) of
        (LKBool l, LKBool r) -> return $ LKBool (l && r)
        _                          -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo

evalExpr (EOr expr1 expr2) = do
    (left, right) <- evalExpr2 expr1 expr2
    case (left, right) of
        (LKBool l, LKBool r) -> return $ LKBool (l || r)
        _                            -> throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo

evalExpr (EString str) = return $ LKString str

evalExpr (ELitInt int) = return $ LKInt int

evalExpr ELitTrue  = return $ LKBool True

evalExpr ELitFalse = return $ LKBool False

evalExpr (ELambda type_ lambdaArgs block) = do
    env <- ask

    let args = Prelude.map lambSuppliedArgToArg lambdaArgs

    return $ LKFunction (LKFunctionDef type_ (Ident "") args block) env


evalExpr (EAppLambda expr exprs) = do
    func <- evalExprWithErrorLogging expr
    evalFunctionApplication func exprs



evalFunctionApplication :: (?evalStmts :: [Stmt] -> Eval ()) => LKValue -> [Expr] -> Eval LKValue
evalFunctionApplication  (LKFunction (LKFunctionDef returnType _ args (Block stmts)) env) exprs = do
    suppliedArgs <- mapM evalExprWithErrorLogging exprs

    unless (length suppliedArgs == length args)
        (throwError $ initRuntimeErrorNoLocation REInvalidNumberOfArgumentsSupplied)

    unless (all (\(suppliedArg, functionArg) -> (lkType suppliedArg == getTypeFromArg functionArg)) (zip suppliedArgs args))
        (throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo)

    updatedEnv <- getUpdatedEnvFromSuppliedExprsAndDefinedFuncsArgs env exprs args

    local (const (increaseLevel updatedEnv)) (?evalStmts stmts >> checkIfFunctionShouldReturnSomething returnType)
            `catchError` catchReturn returnType

evalFunctionApplication LKNotInitFunction _ = throwError $ initRuntimeErrorNoLocation REFunctionNotInitialized
evalFunctionApplication _ _ = throwError $ initRuntimeErrorNoLocation RErrorInvalidTypeNoInfo


getUpdatedEnvFromSuppliedExprsAndDefinedFuncsArgs :: (?evalStmts :: [Stmt] -> Eval ()) => Env -> [Expr] -> [Arg] -> Eval Env
getUpdatedEnvFromSuppliedExprsAndDefinedFuncsArgs env suppliedExprs defFuncsArgs = do
    let (vargs, rargs) = partition (\(_, functionArg) -> isVArg functionArg) (zip suppliedExprs defFuncsArgs)
    let (vFargs, rFargs) = join bimap (Prelude.map snd) (vargs, rargs)
    let (vIdents, rIdents) = join bimap (Prelude.map getIdentFromArg) (vFargs, rFargs)

    vSuppliedArgs <- Prelude.mapM (evalExprWithErrorLogging . fst) vargs
    newVLocs <- Prelude.mapM copySimpleVariable vSuppliedArgs

    rSuppliedArgs <- Prelude.mapM (evalLValueToIdent . fst) rargs
    rLocs <- Prelude.mapM extractVariableLocation rSuppliedArgs

    return $ foldr updateEnv env (zip rLocs rIdents ++ zip newVLocs vIdents)


evalExpr2 :: (?evalStmts :: [Stmt] -> Eval ()) =>  Expr -> Expr -> Eval (LKValue, LKValue)
evalExpr2 leftExpr rightExpr = do
    leftValue  <- evalExprWithErrorLogging leftExpr
    rightValue <- evalExprWithErrorLogging rightExpr
    return (leftValue, rightValue)
        
        
evalLValueToIdent :: (?evalStmts :: [Stmt] -> Eval ()) => Expr -> Eval Ident
evalLValueToIdent (EVar lvalue) = evalLValue lvalue
evalLValueToIdent loc             = throwError $ initRuntimeError RENotLValue loc


evalLValue :: LValue -> Eval Ident
evalLValue (LValue n) = return n
