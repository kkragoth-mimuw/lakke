module Typechecker.Statements where

import Control.Monad.Reader
import Control.Monad.Except

import AbsLakke

import Typechecker.Environment
import Typechecker.TypecheckMonad
import Typechecker.Errors

typecheckTopDefs :: [TopDef] -> TCM TCMEnv
typecheckTopDefs [] = ask
typecheckTopDefs (x:xs) = do
    env <- typecheckTopDef x
    local (const env) (typecheckTopDefs xs)

typecheckTopDef :: TopDef -> TCM TCMEnv
typecheckTopDef (Global decl) = typecheckDecl (DeclS decl) >> ask
typecheckTopDef (FnDef fnDef) = typecheckDecl (DeclF fnDef) >> ask

typecheckDecl (DeclS (Decl type_ (Init lvalue expr))) = undefined


typecheckExprWithErrorLogging :: Expr -> TCM Type
typecheckExprWithErrorLogging expr = typecheckExpr expr `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError expr))

typecheckExpr :: Expr -> TCM Type
typecheckExpr (EString _) = return Str
typecheckExpr (ELitInt _) = return Int
typecheckExpr (ELitTrue) = return Bool
typecheckExpr (ELitFalse) = return Bool
typecheckExpr (EOr expr1 expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Bool, Bool) -> return Bool
        (Bool, x) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpression (EAnd expr1 expr2) = do
    (left, right) <- typecheckExpression2
    case (left, right) of
        (Bool, Bool) -> return Bool
        (Bool, x) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (Not expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Bool -> return Bool
        x -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (Neg expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Int -> return Int
        x -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr (EMul exprLeft _ exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right) of
        (Int, Int) -> return Int
        (Int, x) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _) ->  throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr (ERel exprLeft _ exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right) of
        (String, String) -> return Bool
        (Int, Int) -> return Bool
        (Int, x) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (String, x) ->  throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (x, _) -> throwError $ initTypecheckError $ ICinvalidTypeExpectedTypes x [Int, Str]
typecheckExpr(ECast type_ expr) = do
    exprType <- typecheckExpr expr
    case (type_, exprType) of
        (Str, Int) -> Str
        (Str, x) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
typecheckExpr(EAdd expr1 addop expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Str, Str, Plus) -> Str
        (Int, Int, _) -> Int
        (Str, x, Plus) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (Str, )
typecheckExpr2 :: Expr -> Expr -> TCM (Type, Type)
typecheckExpr2 leftExpr rightExpr = do
    leftType <- typecheckExprWithErrorLogging leftExpr
    rightType <- typecheckExprWithErrorLogging rightExpr
    return (leftType, rightType)