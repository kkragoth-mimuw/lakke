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

typecheckExpr2 :: Expr -> Expr -> TCM (Type, Type)
typecheckExpr2 leftExpr rightExpr = do
    leftType <- typecheckExprWithErrorLogging leftExpr
    rightType <- typecheckExprWithErrorLogging rightExpr
    return (leftType, rightType)