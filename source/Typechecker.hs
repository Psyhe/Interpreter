{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Typechecker where

import           Control.Monad.Except
import           Control.Monad.State
import           Prelude
import           AbsWyso
import           TypecheckerTypes
import           Exceptions
import qualified Data.Map as Map
import           TypecheckerEnv
import Data.List (nub)

type TypecheckerMonad = TypecheckerMonad' ()
type TypecheckerMonad' a = StateT Env (Except TypecheckerException) a

class Typechecker a where
  typecheck :: Maybe PureType -> a -> TypecheckerMonad
  typeget :: a -> TypecheckerMonad' PureType

checkType :: Program -> Either TypecheckerException ()
checkType program =
  runExcept $ evalStateT (typecheck Nothing program) emptyEnv

instance Typechecker Program where
  typecheck _ (ProgramName position alldefs) = do
    validateAllDefs position alldefs
    mapM_ (typecheck Nothing) alldefs

    typecheck Nothing (SExp position (EApp position (Ident "main") []))

instance Typechecker AllDef where
  typecheck _ (VarDef position t name expr) = do
    env <- get
    let pureType = getPureType t
    validateTypeOrThrowChecker position env pureType expr
    assertNoVoidOrFunTypesOrThrowException position [pureType]

    put $ updateType name pureType env

  typecheck _ (FunDef position t name args block) = do
    validateFunctionArgumentsOrThrowException position args
    validateNameAndArguments position name args

    -- We need to add the function to the environment before checking the block.
    env <- get
    let funType = FunType (getTypesFromArgs args) (getPureType t)
    let argPureTypes = getTypesFromArgs args
    assertNoVoidOrFunTypesOrThrowException position argPureTypes

    let rawReturnType = getPureType t
    put $ updateType name funType env

    -- We need to also put the arguments in the environment so that they are visible in the block.
    envWithFun <- get
    let argWithTypes = getArgumentsWithTypes args
    put $ updateTypeList argWithTypes envWithFun

    -- We check the block with the expected return type and we asser that return statement is present.
    typecheck (Just rawReturnType) block
    envAfter <- get
    assertTrueOrThrowExceptionTypechecker (getDidReturn envAfter) (NoReturnStatementException position)

    put $ setDidReturn False envAfter

instance Typechecker Block where
  typecheck expectedRetType (BlockName position stmts) = do
    env <- get
    let oldidentifiers = getIdentifiers env
    let oldDidReturn = getDidReturn env
    mapM_ (typecheck expectedRetType) stmts

    env <- get

    let newDidReturn = (getDidReturn env || oldDidReturn)

    put $ updateEnv oldidentifiers newDidReturn env
  

instance Typechecker Stmt where
  typecheck _ (Empty _) = pure ()

  typecheck expectedReturnType (BStmt _ block) = do
    env <- get
    typecheck expectedReturnType block
    put env

  typecheck expectedReturnType (DefStmt _ def) = do
    typecheck expectedReturnType def

  typecheck _ (Ass position name expr) = do
    env <- get
    case getPureTypeFromEnv name env of
      Just pureType -> validateTypeOrThrowChecker position env pureType expr
      Nothing -> throwError $ UndefinedIdentException position name

  typecheck _ (Decr position name) = 
    expectGivenTypeOrThrowException position IntType name

  typecheck _ (Incr position name) = 
    expectGivenTypeOrThrowException position IntType name

  typecheck (Just expectedReturnType) (Ret position returnExpr) = do
      env <- get
      let typecheckResult = validateTypeM position expectedReturnType returnExpr env
      case typecheckResult of
          Right _ -> pure ()
          Left exception -> throwError exception
      put $ setDidReturn True env

  typecheck Nothing (Ret position _) = 
    throwError $ ReturnOutOfScopeException position

  typecheck (Just expectedReturnType) (VRet position) = do
    env <- get
    assertTrueOrThrowExceptionTypechecker (expectedReturnType == VoidType) (InvalidReturnTypeException position expectedReturnType)
    put $ setDidReturn True env

  typecheck Nothing (VRet position) =
    throwError $ ReturnOutOfScopeException position

  typecheck _ (Cond position expr stmt) = do
    env <- get
    validateTypeOrThrowChecker position env BoolType expr
    typecheck Nothing stmt

  typecheck _ (CondElse position expr stmt1 stmt2) = do
    env <- get
    validateTypeOrThrowChecker position env BoolType expr
    typecheck Nothing stmt1
    typecheck Nothing stmt2

  typecheck _ (While position expr stmt) = do
    env <- get
    validateTypeOrThrowChecker position env BoolType expr
    typecheck Nothing stmt

  typecheck _ (SExp position expr) = do
    env <- get
    validateTypeOrThrowChecker position env VoidType expr

-- We extract the type of the expression and compare it with the expected type.
instance Typechecker Expr where
  typeget (ELitInt _ _) = pure IntType

  typeget (ELitTrue _) = pure BoolType

  typeget (ELitFalse _) = pure BoolType

  typeget (EString _ _) = pure StringType

  typeget (Neg position expr) = do
    validateTypeOrThrowGetter position IntType expr
    pure IntType

  typeget (Not position expr) = do
    validateTypeOrThrowGetter position BoolType expr
    pure BoolType

  typeget (EMul position expr1 _ expr2) = do
    validateTypesOrThrowGetter position IntType expr1 expr2
    pure IntType

  typeget (EAdd position expr1 _ expr2) = do
    validateTypesOrThrowGetter position IntType expr1 expr2
    pure IntType

  typeget (EConcat position expr1 expr2) = do
    validateTypesOrThrowGetter position StringType expr1 expr2
    pure StringType

  typeget (ERel position expr1 _ expr2) = do
    validateIsNotTypeOrThrowGetter position VoidType expr1
    assertSameTypesOrThrowGetter position expr1 expr2
    pure BoolType

  typeget (EAnd position expr1 expr2) = do
    validateTypesOrThrowGetter position BoolType expr1 expr2
    pure BoolType

  typeget (EOr position expr1 expr2) = do
    validateTypesOrThrowGetter position BoolType expr1 expr2
    pure BoolType

  typeget (EVar position name) = do
    env <- get
    case getTypeVar name env of
      Just pureType -> pure pureType
      Nothing      -> throwError $ UndefinedIdentException position name

  typeget (EApp position name arguments) = do
    env <- get
    case getTypeVar name env of
      Just (FunType expectedArgsTypes returnType) -> do
        getFunctionTypeOrThrowException position arguments (FunType expectedArgsTypes returnType)
      Nothing -> throwError $ UndefinedIdentException position name

validateOneOfTwoTypes :: Typechecker a => BNFC'Position -> PureType -> PureType -> a -> TypecheckerMonad
validateOneOfTwoTypes position expectedType1 expectedType2 expr = do
  exprType <- typeget expr
  assertTrueOrThrowExceptionTypechecker (expectedType1 == exprType || expectedType2 == exprType) (InvalidTypeException position expectedType1 exprType)

getName :: AllDef -> Ident
getName (FunDef _ _ name _ _) = name
getName (VarDef _ _ name _) = name

getAllDefNames :: [AllDef] -> [Ident]
getAllDefNames = map getName

areNamesUnique :: Eq a => [a] -> Bool
areNamesUnique names = length names == length (nub names)

getNames :: [(Ident, PureType)] -> [Ident]
getNames = map fst

getTypes :: [(Ident, PureType)] -> [PureType]
getTypes = map snd

getTypeVar :: Ident -> Env -> Maybe PureType
getTypeVar name env = Map.lookup name (identifiers env)

getArgumentsWithTypes :: [Arg] -> [(Ident, PureType)]
getArgumentsWithTypes = map getArgumentWithType

getTypesFromArgs :: [Arg] -> [PureType]
getTypesFromArgs = getTypes . getArgumentsWithTypes

getArgumentWithType :: Arg -> (Ident, PureType)
getArgumentWithType (ValArg _ t name) = (name, getPureType t)
getArgumentWithType (VarArg _ t name) = (name, getPureType t)

validateAllDefs :: BNFC'Position -> [AllDef] -> TypecheckerMonad
validateAllDefs position alldefs = do
  let namesAreValid = areNamesUnique (getAllDefNames alldefs)
  assertTrueOrThrowExceptionTypechecker namesAreValid (NamesDuplicationException position)

validateFunctionArgumentsOrThrowException :: BNFC'Position -> [Arg] -> TypecheckerMonad
validateFunctionArgumentsOrThrowException position arguments = do
  let areArgumentsValid = areNamesUnique (getNames $ getArgumentsWithTypes arguments)
  assertTrueOrThrowExceptionTypechecker areArgumentsValid (NamesDuplicationException position)

assertTypesAreEqual :: PureType -> PureType -> TypecheckerException -> TypecheckerMonad
assertTypesAreEqual expectedType actualType exception = assertTrueOrThrowExceptionTypechecker (expectedType == actualType) exception

validateTypeOrThrowChecker :: Typechecker a => BNFC'Position -> Env -> PureType -> a -> TypecheckerMonad
validateTypeOrThrowChecker position env expectedType expr = do
  let typecheckResult = validateTypeM position expectedType expr env
  case typecheckResult of
    Right _ -> pure ()
    Left exception -> throwError exception

expectGivenTypeOrThrowException :: BNFC'Position -> PureType -> Ident -> TypecheckerMonad
expectGivenTypeOrThrowException position expectedType name = do
  env <- get
  case getPureTypeFromEnv name env of
    Just pureType -> assertTypesAreEqual expectedType pureType (InvalidTypeException position expectedType pureType)
    Nothing -> throwError $ UndefinedIdentException position name

validateTypeM :: Typechecker a => BNFC'Position -> PureType -> a -> Env -> Either TypecheckerException ()
validateTypeM position expectedType expr env =
  runExcept $ evalStateT (validateTypeOrThrowGetter position expectedType expr) env

assertTrueOrThrowExceptionTypechecker :: Bool -> TypecheckerException -> TypecheckerMonad
assertTrueOrThrowExceptionTypechecker True _ = return ()
assertTrueOrThrowExceptionTypechecker False exception = throwError exception

validateTypesOrThrowGetter :: Typechecker a => BNFC'Position -> PureType -> a -> a -> TypecheckerMonad
validateTypesOrThrowGetter position expectedType expr1 expr2 = do
  validateTypeOrThrowGetter position expectedType expr1
  validateTypeOrThrowGetter position expectedType expr2

assertSameTypesOrThrowGetter :: Typechecker a => BNFC'Position -> a -> a -> TypecheckerMonad
assertSameTypesOrThrowGetter position expr1 expr2 = do
  expr1Type <- typeget expr1
  expr2Type <- typeget expr2
  assertTrueOrThrowExceptionTypechecker (expr1Type == expr2Type) (InvalidTypeException position expr1Type expr2Type)

validateTypeOrThrowGetter :: Typechecker a => BNFC'Position -> PureType -> a -> TypecheckerMonad
validateTypeOrThrowGetter position expectedType expr = do
  exprType <- typeget expr
  assertTrueOrThrowExceptionTypechecker (expectedType == exprType) (InvalidTypeException position expectedType exprType)

validateIsNotTypeOrThrowGetter :: Typechecker a => BNFC'Position -> PureType -> a -> TypecheckerMonad
validateIsNotTypeOrThrowGetter position expectedType expr = do
  exprType <- typeget expr
  assertTrueOrThrowExceptionTypechecker (expectedType /= exprType) (VoidVariableException position)

getFunctionTypeOrThrowException :: Typechecker a => BNFC'Position -> [a] -> PureType -> TypecheckerMonad' PureType
getFunctionTypeOrThrowException position actualArgs (FunType expectedArgTypes returnType) = do
  actualArgsTypes <- mapM typeget actualArgs
  expectFunctionArgumentsOrThrowException position expectedArgTypes actualArgsTypes
  pure returnType

expectFunctionArgumentsOrThrowException :: BNFC'Position -> [PureType] -> [PureType] -> TypecheckerMonad
expectFunctionArgumentsOrThrowException position expectedArguments actualArguments = assertTrueOrThrowExceptionTypechecker isValidType exception
  where
    isValidType = expectedArguments == actualArguments
    exception = InvalidFunctionArgumentsTypesException position expectedArguments actualArguments

assertNoVoidOrFunTypesOrThrowException :: BNFC'Position -> [PureType] -> TypecheckerMonad
assertNoVoidOrFunTypesOrThrowException position types = do
  let hasVoidOrFunTypes = any (\t -> t == VoidType || t == FunType [] VoidType) types
  assertTrueOrThrowExceptionTypechecker (not hasVoidOrFunTypes) (VoidVariableException position)

validateNameAndArguments :: BNFC'Position -> Ident -> [Arg] -> TypecheckerMonad
validateNameAndArguments position name args = do
  let names = getNames $ getArgumentsWithTypes args
  assertTrueOrThrowExceptionTypechecker (name `notElem` names) (NamesDuplicationException position)