module Typechecker.TopDefs where

import Control.Monad.Except
import AbsLakke

import Typechecker.Environment
import Typechecker.TypecheckMonad

typecheckTopDefs :: [TopDef] -> TCM TCMEnv
typecheckTopDefs [] = ask
typecheckTopDefs (x:xs) = do
    env <- typecheckTopDef x
    local (const env) (typecheckTopDefs xs)

typecheckTopDef :: TopDef -> TCM TCMEnv
typecheckTopDef (Global decl) = typecheckDecl (DeclS decl)
typecheckTopDef (FnDef fnDef) = typecheckDecl (DeclF fnDef)

typecheckDecl (DeclS (Decl type_ (Init lvalue expr))) = undefined


typecheckExprWithErrorLogging :: Expr -> TCM Type
typecheckExprWithErrorLogging expr = typecheckExpression expr `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError expr))

typecheckExpression :: Expr -> TCM Type
typecheckExpression (EString _) = return Str
typecheckExpression (ELitInt _) = return Int
typecheckExpression (ELitTrue) = return Bool
typecheckExpression (ELitFalse) = return Bool
typecheckExpression (EOr expr1 expr2) = do
    (left, right) <- evalExpr
    case (left, right) of
        (Bool, Bool) -> return Bool
        (Bool, x) -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool

typecheckExpression2 :: Expr -> Expr -> TCM (Type, Type)
typecheckExpression2 leftExpr rightExpr = do
    leftType <- typecheckExpressionWithErrorLogging leftExpr
    rightType <- typecheckExpressionWithErrorLogging rightExpr
    return (leftType, rightType)