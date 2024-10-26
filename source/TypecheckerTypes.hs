module TypecheckerTypes where

import           AbsWyso
import           Prelude                 

data PureType 
    = IntType
    | BoolType
    | StringType
    | VoidType
    | FunType [PureType] PureType
    deriving (Eq)

instance Show PureType where
    show IntType = "int"
    show BoolType = "bool"
    show StringType = "string"
    show VoidType = "void"
    show (FunType args ret) = "def(" ++ unwords (map show args) ++ ") -> " ++ show ret

getPureType :: Type -> PureType
getPureType (Int _) = IntType
getPureType (Bool _) = BoolType
getPureType (Str _) = StringType
getPureType (Void _) = VoidType
getPureType (Fun _ args ret) = FunType (map getPureType args) (getPureType ret)

getPureFromFunction :: [Arg] -> Type -> PureType
getPureFromFunction args ret = FunType (getPureArgs args) (getPureType ret)

getPureArgs :: [Arg] -> [PureType]
getPureArgs = map getPureArg

getPureArg :: Arg -> PureType
getPureArg (ValArg _ t _) = getPureType t
getPureArg (VarArg _ t _) = getPureType t

isInt :: PureType -> Bool
isInt IntType = True
isInt _ = False

isBool :: PureType -> Bool
isBool BoolType = True
isBool _ = False

isString :: PureType -> Bool
isString StringType = True
isString _ = False

isVoid :: PureType -> Bool
isVoid VoidType = True
isVoid _ = False

isFunction :: PureType -> Bool
isFunction (FunType _ _) = True
isFunction _ = False