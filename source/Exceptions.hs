{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Exceptions where

import           AbsWyso
import           TypecheckerTypes

type RuntimeException = RuntimeException' BNFC'Position

data RuntimeException' a
  = OtherRuntimeException a
  | DivideByZeroException a
    deriving (Eq)

instance Show RuntimeException where
  show (OtherRuntimeException position) =
    "RUNTIME EXCEPTION: Other runtime exception " ++ showPos position

  show (DivideByZeroException position) =
    "RUNTIME EXCEPTION: Tried dividing by zero " ++ showPos position

type TypecheckerException = TypecheckerException' BNFC'Position

data TypecheckerException' a
  = UndefinedIdentException a Ident
  | InvalidTypeException a PureType PureType
  | InvalidFunctionArgumentsTypesException a [PureType] [PureType]
  | InvalidReturnTypeException a PureType
  | ReturnOutOfScopeException a
  | NoReturnStatementException a
  | NamesDuplicationException a
  | VoidVariableException a
  | OtherException a
    deriving (Eq)


instance Show TypecheckerException where
  show (UndefinedIdentException position name) = concat [
    "TYPECHECKER EXCEPTION: Undefined ident: ", showIdent name, " ", showPos position
    ]

  show (InvalidTypeException position expectedType actualType) = concat [
    "TYPECHECKER EXCEPTION: Invalid type. Expected: ", show expectedType, ", got: ", show actualType, " ", showPos position
    ]

  show (InvalidFunctionArgumentsTypesException position expectedTypes actualTypes) = concat [
    "TYPECHECKER EXCEPTION: Invalid function arguments. Expected: ", show expectedTypes, ", got: ", show actualTypes, " ", showPos position
    ]

  show (InvalidReturnTypeException position expectedType) = concat [
    "TYPECHECKER EXCEPTION: Invalid function return type. Expected: ", show expectedType, " ", showPos position
    ]

  show (ReturnOutOfScopeException position) =
    "TYPECHECKER EXCEPTION: Return statement out of scope " ++ showPos position

  show (NoReturnStatementException position) =
    "TYPECHECKER EXCEPTION: Block has no return statement " ++ showPos position

  show (NamesDuplicationException position) =
    "TYPECHECKER EXCEPTION: Names duplication " ++ showPos position

  show (VoidVariableException position) =
    "TYPECHECKER EXCEPTION: Void variable " ++ showPos position

  show (OtherException position) =
    "TYPECHECKER EXCEPTION: Other typechecking exception " ++ showPos position

showPos :: BNFC'Position -> String
showPos (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c
showPos Nothing = "unknown position"

showIdent :: Ident -> String
showIdent (Ident name) = name

showArg :: Arg -> String
showArg (ValArg _ t name) = showType t ++ " " ++ showIdent name
showArg (VarArg _ t name) = "var " ++ showType t ++ " " ++ showIdent name

showType :: Type -> String
showType (Int _) = "int"
showType (Bool _) = "bool"
showType (Str _) = "string"
showType (Void _) = "void"
showType (Fun _ args ret) = show ret ++ " (" ++ show args ++ ")"
