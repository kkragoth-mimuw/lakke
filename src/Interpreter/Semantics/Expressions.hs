{-# LANGUAGE LambdaCase, ImplicitParams #-}

module Interpreter.Semantics.Expressions where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                      as Map hiding (foldr)
import           Debug.Trace

import           AbsLakke

import           Interpreter.Debug
import           Interpreter.ErrorTypes
import           Interpreter.EvalMonad
import           Interpreter.DomainsUtils
import           Interpreter.Semantics.Domains
import           Interpreter.Values
import           Interpreter.Utils
import           Interpreter.TypesUtils

evalExprDependencyInjection :: ([Stmt] -> Eval ()) -> Expr -> Eval LKValue
evalExprDependencyInjection evalStmts expr = let ?evalStmts = evalStmts in evalExpr expr

getIdentFromArg :: Arg -> Ident
getIdentFromArg (VArg _ ident) = ident
getIdentFromArg (RArg _ ident) = ident

getTypeFromArg :: Arg -> Type
getTypeFromArg (VArg type_ _) = type_
getTypeFromArg (RArg type_ _) = type_

isVArg :: Arg -> Bool
isVArg (VArg _ _) = True
isVArg _ = False

updateEnv :: (Location, Ident) -> Env -> Env
updateEnv (location, ident) env = env & (varsEnv . at ident ?~ (location, getLevel env))

evalExpr :: (?evalStmts :: [Stmt] -> Eval ()) => Expr -> Eval LKValue
evalExpr (EApp lvalue exprs) = do
    let ?evalStmts = ?evalStmts
    env <- ask
    store <- get
    ident <- evalLValue lvalue

    suppliedArgs <- mapM evalExpr exprs
    case (env & (funcsEnv & view)) ^.at ident of
        Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        Just (loc, _) -> case (store & (funcDefs & view)) ^.at loc of
            Just (LKFunctionDef returnType ident args (Block stmts)) -> do
                 newLocs <- Prelude.mapM copySimpleVariable suppliedArgs

                 unless (length suppliedArgs == length args)
                    (throwError REInvalidNumberOfArgumentsSupplied)

                 unless (all (\(suppliedArg, functionArg) -> (lkType suppliedArg == getTypeFromArg functionArg)) (zip suppliedArgs args))
                    (throwError RErrorInvalidTypeNoInfo)
                 let idents = Prelude.map getIdentFromArg args

                 let newEnv = foldr updateEnv env (zip newLocs idents)

                --  traceM $ show newLocs

                 store <- get

                --  debug newEnv store

                 (do local (const (increaseLevel newEnv)) (?evalStmts stmts)
                     if returnType == Void then
                        return LKVoid
                     else
                        throwError RENoReturnValue
                  )
                 `catchError` (
                     \case
                        LKReturn value -> case value of
                                            Just returnValue -> return returnValue
                                            Nothing -> if returnType == Void then
                                                            return LKVoid
                                                       else
                                                            throwError RENoReturnValue
                        error -> throwError error
                  )
            Nothing  -> throwError RErrorMemoryLocation

evalExpr (ECast type_  expr) = do
    value <- evalExpr expr

    unless (isSimpleType value && checkIfAllowedType simpleTypes type_)
        (throwError $ RErrorInvalidTypeNoInfo)

    case (type_, value) of
        (Str, LKInt value) -> return $ LKString (show value)
        _ -> throwError $ RErrorInvalidTypeNoInfo


evalExpr (EAdd expr1 addop expr2) = do
    (l, r) <- evalExpr2 expr1 expr2

    case (l, r, addop) of
        (LKString l, LKString r, Plus) -> return $ LKString (l ++ r)
        (LKInt l, LKInt r, Plus) -> return $ LKInt (l + r)
        (LKInt l, LKInt r, Minus) -> return $ LKInt (l - r)
        _ -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EVar lvalue) = do
    env <- ask
    store <- get
    ident <- evalLValue lvalue

    case (env & (varsEnv & view)) ^.at ident of
        Nothing -> throwError $ RErrorUnknownIdentifier (show ident)
        Just (loc, _) -> case (store & (vars & view)) ^.at loc of
            Just var -> return var
            Nothing  -> throwError RErrorMemoryLocation


evalExpr rel@(ERel exprLeft relOp exprRight) = do
    eLeft <- evalExpr exprLeft
    eRight <- evalExpr exprRight

    case (eLeft, eRight) of
        (LKString l, LKString r) -> return $ LKBool ((mapRelOpToRelFunction relOp) l r)
        (LKInt l, LKInt r) -> return $ LKBool ((mapRelOpToRelFunction relOp) l r)
        _ -> throwError $ RErrorInvalidType Int "" rel

evalExpr mul@(EMul exprLeft mulOp exprRight) = do
    eLeft <- evalExpr exprLeft
    eRight <- evalExpr exprRight
    case (eLeft, eRight, mulOp) of
        (LKInt l, LKInt 0, Div ) -> throwError $ RErrorDivisonByZero
        (LKInt l, LKInt r, _) -> return $ LKInt ((mapMulOpToMulFunction mulOp) l r)
        _ -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EString str) = return $ LKString str
evalExpr (ELitInt int) = return $ LKInt int
evalExpr ELitTrue  = return $ LKBool True
evalExpr ELitFalse = return $ LKBool False

evalExpr (Neg expr) = do
    var <- evalExpr expr
    case var of 
        (LKInt value) -> return $ LKInt (negate value)
        _ -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (Not expr) = do
    cond <- evalExpr expr
    case cond of
        (LKBool value) -> return $ LKBool (not value)
        _ -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EAnd expr1 expr2) = do
    (left, right) <- evalExpr2 expr1 expr2
    case (left, right) of
        (LKBool True, LKBool True) -> return $ LKBool True
        (LKBool _, LKBool _) -> return $ LKBool False
        _ -> throwError $ RErrorInvalidTypeNoInfo

evalExpr (EOr expr1 expr2) = do
    (left, right) <- evalExpr2 expr1 expr2
    case (left, right) of
        (LKBool False, LKBool False) -> return $ LKBool False
        (LKBool _, LKBool _) -> return $ LKBool True
        _ -> throwError $ RErrorInvalidTypeNoInfo


evalExpr2 :: (?evalStmts :: [Stmt] -> Eval ()) =>  Expr -> Expr -> Eval (LKValue, LKValue)
evalExpr2 leftExpr rightExpr = do 
    leftValue  <- evalExpr leftExpr 
    rightValue <- evalExpr rightExpr
    return (leftValue, rightValue)


evalLValue :: LValue -> Eval Ident
evalLValue (LValue n) = return n

