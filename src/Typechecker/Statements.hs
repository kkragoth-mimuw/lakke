module Typechecker.Statements where

import           Control.Lens                 hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader

import           AbsLakke

import           Typechecker.Environment
import           Typechecker.EnvironmentUtils
import           Typechecker.Errors
import           Typechecker.TypecheckMonad

typecheckTopDefs :: [TopDef] -> TCM TCMEnv
typecheckTopDefs [] = ask
typecheckTopDefs (x:xs) = do
    env <- typecheckTopDef x
    local (const env) (typecheckTopDefs xs)

typecheckTopDef :: TopDef -> TCM TCMEnv
typecheckTopDef (Global decl) = typecheckDeclWithLogging (DeclS decl)
typecheckTopDef (FnDef fnDef) = typecheckDeclWithLogging (DeclF fnDef)

typecheckDeclWithLogging :: Stmt -> TCM TCMEnv
typecheckDeclWithLogging stmt = typecheckDecl stmt `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError stmt))

typecheckDecl :: Stmt -> TCM TCMEnv
typecheckDecl (DeclS (Decl type_ (Init lvalue expr))) = do
    exprType <- typecheckExpr expr
    name <- evalLValue lvalue

    checkIfIsAlreadyDeclaredAtCurrentLevel name

    when (type_ /= exprType)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType type_)

    env <- ask

    return (env & (tcmTypes . at name ?~ (type_, getLevel env)))


typecheckDecl (DeclS (Decl type_ (NoInit lvalue))) = do
    name <- evalLValue lvalue

    checkIfIsAlreadyDeclaredAtCurrentLevel name

    env <- ask

    return (env & (tcmTypes . at name ?~ (type_, getLevel env)))

typecheckDecl (DeclF (FNDef fnType fnName args (Block stmts))) = do
    checkIfIsAlreadyDeclaredAtCurrentLevel fnName

    env <- ask

    let funcType = LambdaType (Prelude.map argToLambArg args)  fnType

    let newEnv = (env  & (tcmTypes . at fnName ?~ (funcType, getLevel env)))

    let newEnvForFunction = newEnv

    local (const newEnvForFunction) (typecheckStmts stmts)

    return newEnv

argToLambArg :: Arg -> LambArg
argToLambArg arg = case arg of
    VArg type' _ -> LambVArg type'
    RArg type' _ -> LambRArg type'


typecheckStmts :: [Stmt] -> TCM ()
typecheckStmts [] = return ()
typecheckStmts (x:xs) = do
    env <- typecheckStmtOrDeclaration x
    local (const env) (typecheckStmts xs)

typecheckStmtOrDeclaration :: Stmt -> TCM TCMEnv
typecheckStmtOrDeclaration stmt =
    if isStmtDeclaration stmt then
        typecheckDeclWithLogging stmt
    else
        typecheckStmtWithLogging stmt >> ask


typecheckStmtWithLogging :: Stmt -> TCM ()
typecheckStmtWithLogging stmt = typecheckStmt stmt `catchError` (\typecheckError -> throwError (appendLogToTypecheckError typecheckError stmt))

typecheckStmt :: Stmt -> TCM ()
typecheckStmt (Cond expr block@(Block blockTrue)) = typecheckStmt (CondElse expr block (Block []))

typecheckStmt (CondElse expr (Block blockTrue) (Block blockFalse)) = do
    exprType <- typecheckExprWithErrorLogging expr

    unless (exprType == Bool)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType exprType Bool)

    typecheckStmts blockTrue
    typecheckStmts blockFalse

typecheckStmt (While expr block) = typecheckStmt (For Empty expr Empty block)

typecheckStmt (For initStmt expr outerStmt block@(Block stmts)) = do
    env <- typecheckStmtOrDeclaration initStmt

    local (const (indicateLoop env)) (
        do
            conditionType <- typecheckExpr expr

            case conditionType of
                Bool -> do
                    typecheckStmts stmts
                    typecheckStmt outerStmt
                x -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        )

typecheckStmt (Ass lvalue expr) = do
    ident <- evalLValue lvalue
    lvalueType <- extractVariableType ident
    rvalueType <- typecheckExpr expr

    when (lvalueType /= rvalueType)
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedType rvalueType lvalueType)

typecheckStmt Empty = return ()

typecheckStmt Break = do
    env <- ask

    unless (not $ isInLoop env)
        (throwError $ initTypecheckError TCBreak)

typecheckStmt Continue = do
    env <- ask

    unless (not $ isInLoop env)
        (throwError $ initTypecheckError TCContinue)

typecheckStmt (SExp expr) = typecheckExpr expr >> return ()

typecheckStmt (BStmt (Block stmts)) = local increaseLevel (typecheckStmts stmts)

typecheckStmt VRet = do
    env <- ask

    case getReturnType env of
        Nothing -> throwError $ initTypecheckError TCReturn
        Just Void -> return ()
        Just t -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType Void t

typecheckStmt (Print expr) = do
    exprType <- typecheckExpr expr

    unless (exprType `notElem` [Str, Bool, Int])
        (throwError $ initTypecheckError $ TCInvalidTypeExpectedTypes exprType [Str, Bool, Int])

typecheckStmt _ = undefined

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
        (Bool, x)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)       -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (EAnd expr1 expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right) of
        (Bool, Bool) -> return Bool
        (Bool, x)    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
        (x, _)       -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (Not expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Bool -> return Bool
        x    -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Bool
typecheckExpr (Neg expr) = do
    exprType <- typecheckExprWithErrorLogging expr
    case exprType of
        Int -> return Int
        x   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr (EMul exprLeft _ exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right) of
        (Int, Int) -> return Int
        (Int, x)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _)     ->  throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
typecheckExpr (ERel exprLeft _ exprRight) = do
    (left, right) <- typecheckExpr2 exprLeft exprRight
    case (left, right) of
        (Str, Str) -> return Bool
        (Int, Int) -> return Bool
        (Int, x)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (Str, x)   ->  throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (x, _)     -> throwError $ initTypecheckError $ TCInvalidTypeExpectedTypes x [Int, Str]
typecheckExpr(ECast type_ expr) = do
    exprType <- typecheckExpr expr
    case (type_, exprType) of
        (Str, Int) -> return Str
        (Str, x)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _)     -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
typecheckExpr(EAdd expr1 addop expr2) = do
    (left, right) <- typecheckExpr2 expr1 expr2
    case (left, right, addop) of
        (Str, Str, Plus) -> return Str
        (Int, Int, _)    -> return Int
        (Str, x, Plus)   -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Str
        (Int, x, _)      -> throwError $ initTypecheckError $ TCInvalidTypeExpectedType x Int
        (x, _, _)        -> throwError $ initTypecheckError $ TCInvalidTypeExpectedTypes x [Int, Str]
typecheckExpr2 :: Expr -> Expr -> TCM (Type, Type)
typecheckExpr2 leftExpr rightExpr = do
    leftType <- typecheckExprWithErrorLogging leftExpr
    rightType <- typecheckExprWithErrorLogging rightExpr
    return (leftType, rightType)


evalLValueToIdent :: Expr -> TCM Ident
evalLValueToIdent (EVar lvalue) = evalLValue lvalue
evalLValueToIdent loc           = throwError $ initTypecheckError $ TCMNotLValue


evalLValue :: LValue -> TCM Ident
evalLValue (LValue n) = return n


isStmtDeclaration :: Stmt -> Bool
isStmtDeclaration stmt = case stmt of
    (DeclS _ ) -> True
    (DeclF _ ) -> True
    _          -> False
