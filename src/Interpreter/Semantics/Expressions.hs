{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase     #-}

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

import           Interpreter.Debug
import           Interpreter.DomainsUtils
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.Semantics.Domains
import           Interpreter.TypesUtils
import           Interpreter.Utils
import           Interpreter.Values
import           Interpreter.FuncUtils

evalExprDependencyInjection :: ([Stmt] -> Eval ()) -> Expr -> Eval LKValue
evalExprDependencyInjection evalStmts expr = let ?evalStmts = evalStmts in evalExpr expr


evalExpr :: (?evalStmts :: [Stmt] -> Eval ()) => Expr -> Eval LKValue
evalExpr (EApp lvalue exprs) = do
    ident <- evalLValue lvalue
    suppliedArgs <- mapM evalExpr exprs

    (LKFunctionDef returnType _ args (Block stmts), env) <- extractFunction ident

    unless (length suppliedArgs == length args)
        (throwError REInvalidNumberOfArgumentsSupplied)

    unless (all (\(suppliedArg, functionArg) -> (lkType suppliedArg == getTypeFromArg functionArg)) (zip suppliedArgs args))
        (throwError RErrorInvalidTypeNoInfo)

    updatedEnv <- getUpdatedEnvFromSuppliedExprsAndDefinedFuncsArgs env exprs args

    local (const (increaseLevel updatedEnv)) (?evalStmts stmts >> checkIfFunctionShouldReturnSomething returnType)
             `catchError` catchReturn returnType


evalExpr (ECast type_  expr) = do
    value <- evalExpr expr

    unless (isSimpleType value && checkIfAllowedType simpleTypes type_)
        (throwError $ RErrorInvalidTypeNoInfo)

    case (type_, value) of
        (Str, LKInt value) -> return $ LKString (show value)
        _                  -> throwError $ RErrorInvalidTypeNoInfo


evalExpr (EAdd expr1 addop expr2) = do
    (l, r) <- evalExpr2 expr1 expr2

    case (l, r, addop) of
        (LKString l, LKString r, Plus) -> return $ LKString (l ++ r)
        (LKInt l, LKInt r, Plus)       -> return $ LKInt (l + r)
        (LKInt l, LKInt r, Minus)      -> return $ LKInt (l - r)
        _                              -> throwError $ RErrorInvalidTypeNoInfo


evalExpr (EVar lvalue) = evalLValue lvalue >>= extractVariable

evalExpr rel@(ERel exprLeft relOp exprRight) = do
    (left, right) <- evalExpr2 exprLeft exprRight

    case (left, right) of
        (LKString l, LKString r) -> return $ LKBool (mapRelOpToRelFunction relOp l r)
        (LKInt l, LKInt r)       -> return $ LKBool (mapRelOpToRelFunction relOp l r)
        _                        -> throwError $ RErrorInvalidType Int "" rel

evalExpr mul@(EMul exprLeft mulOp exprRight) = do
    (left, right) <- evalExpr2 exprLeft exprRight
    case (left, right, mulOp) of
        (LKInt l, LKInt 0, Div ) -> throwError $ RErrorDivisonByZero
        (LKInt l, LKInt r, _)    -> return $ LKInt (mapMulOpToMulFunction mulOp l r)
        _                        -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (Neg expr) = do
    var <- evalExpr expr
    case var of
        (LKInt value) -> return $ LKInt (negate value)
        _             -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (Not expr) = do
    cond <- evalExpr expr
    case cond of
        (LKBool value) -> return $ LKBool (not value)
        _              -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EAnd expr1 expr2) = do
    (left, right) <- evalExpr2 expr1 expr2
    case (left, right) of
        (LKBool True, LKBool True) -> return $ LKBool True
        (LKBool _, LKBool _)       -> return $ LKBool False
        _                          -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EOr expr1 expr2) = do
    (left, right) <- evalExpr2 expr1 expr2
    case (left, right) of
        (LKBool False, LKBool False) -> return $ LKBool False
        (LKBool _, LKBool _)         -> return $ LKBool True
        _                            -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EString str) = return $ LKString str

evalExpr (ELitInt int) = return $ LKInt int

evalExpr ELitTrue  = return $ LKBool True

evalExpr ELitFalse = return $ LKBool False


evalLValueToIdent :: (?evalStmts :: [Stmt] -> Eval ()) => Expr -> Eval Ident
evalLValueToIdent (EVar lvalue) = evalLValue lvalue
evalLValueToIdent _             = throwError RENotLValue


evalLValue :: LValue -> Eval Ident
evalLValue (LValue n) = return n


getUpdatedEnvFromSuppliedExprsAndDefinedFuncsArgs :: (?evalStmts :: [Stmt] -> Eval ()) => Env -> [Expr] -> [Arg] -> Eval Env
getUpdatedEnvFromSuppliedExprsAndDefinedFuncsArgs env suppliedExprs defFuncsArgs = do
    let (vargs, rargs) = partition (\(_, functionArg) -> isVArg functionArg) (zip suppliedExprs defFuncsArgs)
    let (vFargs, rFargs) = join bimap (Prelude.map snd) (vargs, rargs)
    let (vIdents, rIdents) = join bimap (Prelude.map getIdentFromArg) (vFargs, rFargs)

    vSuppliedArgs <- Prelude.mapM (evalExpr . fst) vargs
    newVLocs <- Prelude.mapM copySimpleVariable vSuppliedArgs

    rSuppliedArgs <- Prelude.mapM (evalLValueToIdent . fst) rargs
    rLocs <- Prelude.mapM extractVariableLocation rSuppliedArgs

    return $ foldr updateEnv env (zip rLocs rIdents ++ zip newVLocs vIdents)


evalExpr2 :: (?evalStmts :: [Stmt] -> Eval ()) =>  Expr -> Expr -> Eval (LKValue, LKValue)
evalExpr2 leftExpr rightExpr = do
    leftValue  <- evalExpr leftExpr
    rightValue <- evalExpr rightExpr
    return (leftValue, rightValue)



