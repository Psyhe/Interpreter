module EvaluatorTypes where

import           AbsWyso
-- import           EvaluatorEnv

import           Prelude

import qualified Data.Map as Map

type Loc = Integer

newtype Env = Env 
    { envInternal :: Map.Map Ident Loc
    }
    deriving (Eq)

data ValueType
    = IntValue Integer
    | BoolValue Bool
    | StringValue String
    | VoidValue
    | FunValue [Arg] Block Env
    | MockValue
    deriving (Eq)

instance Ord Env where
    compare _ _ = EQ

instance Ord ValueType where
    compare (IntValue i) (IntValue j) = compare i j
    compare (BoolValue b) (BoolValue c) = compare b c
    compare (StringValue s) (StringValue t) = compare s t
    compare VoidValue VoidValue = EQ
    compare (FunValue args _ _) (FunValue args' _ _) = compare args args'
    compare MockValue MockValue = EQ

instance Show ValueType where
    show (IntValue i) = show i
    show (BoolValue b) = show b
    show (StringValue s) = s
    show VoidValue = "void"
    show (FunValue args _ _) = "def(" ++ unwords (map show args) ++ ")"
    show MockValue = "mock"

isMock :: ValueType -> Bool
isMock MockValue = True
isMock _ = False

isTrue :: ValueType -> Bool
isTrue (BoolValue True) = True
isTrue _ = False

isInt :: ValueType -> Bool
isInt (IntValue _) = True

isValueEqualInt :: ValueType -> Integer -> Bool
isValueEqualInt (IntValue i) j = i == j
isValueEqualInt _ _ = False

applyFunctionBool :: (Bool -> Bool) -> ValueType -> ValueType
applyFunctionBool f (BoolValue b) = BoolValue $ f b

applyFunctionInt :: (Integer -> Integer) -> ValueType -> ValueType
applyFunctionInt f (IntValue i) = IntValue $ f i

applyFunctionString :: (String -> String) -> ValueType -> ValueType
applyFunctionString f (StringValue s) = StringValue $ f s

showValueType :: ValueType -> String
showValueType (IntValue i) = show i
showValueType (BoolValue b) = show b
showValueType (StringValue s) = s