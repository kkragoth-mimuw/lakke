{-# LANGUAGE LambdaCase #-}

module Typechecker.Utils where

import           Data.Char

import AbsLakke

argToLambArg :: Arg -> LambArg
argToLambArg arg = case arg of
    VArg type' _ -> LambVArg type'
    RArg type' _ -> LambRArg type'

lambArgToType :: LambArg -> Type
lambArgToType arg = case arg of
    LambVArg type' -> type'
    LambRArg type' -> type'


lambSuppliedArgToArg :: LambSuppliedArgWithType -> Arg
lambSuppliedArgToArg = \case LambSuppliedVArgWithType ident argType -> VArg argType ident
                             LambSuppliedRArgWithType ident argType -> RArg argType ident

lambSuppliedArgToLambArg :: LambSuppliedArgWithType -> LambArg
lambSuppliedArgToLambArg = \case LambSuppliedVArgWithType ident argType -> LambVArg argType
                                 LambSuppliedRArgWithType ident argType -> LambRArg argType

isLambVArg :: LambArg -> Bool
isLambVArg arg = case arg of
    LambVArg _ -> True
    LambRArg _ -> False


trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight str      | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs

trim :: String -> String
trim = trimLeft . trimRight
