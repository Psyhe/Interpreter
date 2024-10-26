
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Evaluator where

import           AbsWyso
import           Control.Monad.Except
import           Control.Monad.State
import           Prelude

import qualified Data.Map as Map

import           Exceptions
import           EvaluatorEnv
import           EvaluatorTypes

type EvaluatorMonad = EvaluatorMonad' ValueType
type EvaluatorMonad' a = StateT EvaluatorState (ExceptT RuntimeException IO) a

class Evaluator a where
  eval :: a -> EvaluatorMonad

evalProgram :: Program -> IO (Either RuntimeException ValueType)
evalProgram program =
  runExceptT $ evalStateT (eval program) emptyEvaluatorState

instance Evaluator Program where
    eval (ProgramName position alldefs) = do
        mapM_ eval alldefs
        eval $ EApp position (Ident "main") []

instance Evaluator AllDef where
    eval (VarDef _ _ name expr) = do
        state <- get
        
        -- Extract the env from the state if needed
        let currentEnv = env state
        -- Evaluate the expression
        value <- eval expr
        -- Modify the state to insert the new value
        modify $ insertNewValueInEvaluatorState name value

        pure MockValue

    eval (FunDef _ _ name args block) = do
        state <- get

        let currentEnv = env state
        -- Just after declaration, I want to add myself to my env and state
        -- chcę mieć w środowisku siebie samego, ale jak to zrobić????


        let funType = FunValue args block currentEnv
        -- modify $ insertEvaluatorStateValue name funType
        modify $ insertNewValueInEvaluatorState name funType

        state <- get
        
        pure MockValue

instance Evaluator Block where
    eval (BlockName _ stmts) = do
        store <- get 
        let oldEnv = env store
        -- liftIO $ putStrLn $ show store
        mapM_ eval stmts
        modify $ insertEnvEvaluatorState oldEnv
        store <- get
        -- liftIO $ putStrLn $ show store
        pure MockValue

instance Evaluator Stmt where
    eval (Empty _) = evalIfNoReturnHappened $ pure MockValue

    eval (BStmt _ block) = evalIfNoReturnHappened (keepEnv (eval block))

    eval (DefStmt _ alldef) = evalIfNoReturnHappened (eval alldef)

    eval (Ass _ ident expr) = evalIfNoReturnHappened $ do
        exprVal <- eval expr
        modify $ updateValueInEvaluatorState ident exprVal
        pure MockValue

    eval (Incr _ ident) = evalIfNoReturnHappened (applyIntFunction (1 +) ident)
    eval (Decr _ ident) = evalIfNoReturnHappened (applyIntFunction (subtract 1) ident)

    eval (Ret _ expr) = evalIfNoReturnHappened $ do
        exprVal <- eval expr
        modify $ updateReturnValue exprVal
        pure MockValue
    
    eval (VRet _ ) = evalIfNoReturnHappened $ do
        modify $ updateReturnValue VoidValue
        pure MockValue

    eval (Cond _ expr block) = evalIfNoReturnHappened $ do
        exprVal <- eval expr
        keepEnv $ 
            if isTrue (exprVal) then eval block else pure MockValue

    eval (CondElse _ expr block1 block2) = evalIfNoReturnHappened $ do
        exprVal <- eval expr
        keepEnv $ 
            if isTrue (exprVal) then eval block1 else eval block2

    eval (While pos expr block) = evalIfNoReturnHappened $ do
        exprVal <- eval expr
        if isTrue exprVal
            then do
                _ <- eval block 
                eval (While pos expr block)
            else pure MockValue

    eval (SExp _ expr) = evalIfNoReturnHappened $ do
        _ <- eval expr
        pure MockValue

instance Evaluator Expr where
    eval (EVar _ name) = do
        state <- get
        pure $ getValueFromEvaluatorState name state 

    eval (ELitInt _ int) = pure $ IntValue int

    eval (ELitTrue _) = pure $ BoolValue True

    eval (ELitFalse _) = pure $ BoolValue False

    eval (EString _ str) = pure $ StringValue str

    eval (Neg _ expr) = do
        value <- eval expr
        pure $ applyFunctionInt negate value

    eval (Not _ expr) = do
        value <- eval expr
        pure $ applyFunctionBool not value

    eval (EMul _ expr1 (Times _) expr2) = evalIntegerExpr (*) expr1 expr2

    eval (EMul position expr1 (Div _) expr2) = do
        expr2Val <- eval expr2
        if isValueEqualInt expr2Val 0 then throwError $ DivideByZeroException position else pure MockValue
        evalIntegerExpr div expr1 expr2

    eval (EMul position expr1 (Mod _) expr2) = do
        expr2Val <- eval expr2
        if isValueEqualInt expr2Val 0 then throwError $ DivideByZeroException position else pure MockValue
        evalIntegerExpr mod expr1 expr2

    eval (EAdd _ expr1 (Plus _) expr2) = evalIntegerExpr (+) expr1 expr2

    eval (EAdd _ expr1 (Minus _) expr2) = evalIntegerExpr (-) expr1 expr2

    eval (ERel _ expr1 (LTH _ ) expr2) = evalComparator (<) expr1 expr2

    eval (ERel _ expr1 (LE _) expr2) = evalComparator (<=) expr1 expr2

    eval (ERel _ expr1 (GTH _) expr2) = evalComparator (>) expr1 expr2

    eval (ERel _ expr1 (GE _) expr2) = evalComparator (>=) expr1 expr2

    eval (ERel _ expr1 (EQU _) expr2) = evalComparator (==) expr1 expr2

    eval (ERel _ expr1 (NE _) expr2) = evalComparator (/=) expr1 expr2

    eval (EConcat _ expr1 expr2) = evalStringExpr (++) expr1 expr2

    eval (EAnd _ expr1 expr2) = do
        expr1Val <- eval expr1
        if isTrue expr1Val
            then do
                expr2Val <- eval expr2
                pure expr2Val
            else pure expr1Val

    eval (EOr _ expr1 expr2) = do
        expr1Val <- eval expr1
        if isTrue expr1Val
            then pure expr1Val
            else do 
                expr2Val <- eval expr2
                pure expr2Val

    eval (EApp _ name exprs) = evalBuildInOrRegularFun name exprs $ do
        state <- get

        -- We need to store the old env.
        let oldEnv = getEnvEvaluatorState state 
        -- We need to compute the arguments and their locations.
        argVals <- mapM eval exprs
        argLocs <- mapM getArgLoc exprs

        -- We need to get the function from the state.
        let localEnv = getFunctionEnv name state 
        let localBlock = getFunctionBlock name state 
        let localArgs = getFunctionArgs name state 
        -- Arguments will be needed to check if the argument was passed by value or by reference

        modify $ insertEnvEvaluatorState localEnv 
        modify $ insertDefaultReturnValue
        -- modify $ updateValueInEvaluatorState name (FunValue localArgs localBlock localEnv)
        modify $ addForRecursionValueInEvaluatorState name (FunValue localArgs localBlock localEnv)
        mapM putArgs (zip3 localArgs argVals argLocs)

        eval localBlock
        state <- get
        let ret = getReturnValue state 
        modify $ insertEnvEvaluatorState oldEnv

        pure ret

evalIfNoReturnHappened :: EvaluatorMonad -> EvaluatorMonad
evalIfNoReturnHappened exec = do
    state <- get
    if isReturnNotDefined state 
        then do
          exec
        else do
          pure MockValue

keepEnv :: EvaluatorMonad -> EvaluatorMonad
keepEnv exec = do
    state <- get
    let env = getEnvEvaluatorState state 
    exec
    modify $ insertEnvEvaluatorState env
    pure MockValue

applyFunctionOnIntType :: (Integer -> Integer) -> ValueType -> ValueType
applyFunctionOnIntType f (IntValue i) = IntValue (f i)

applyIntFunction :: (Integer -> Integer) -> Ident -> EvaluatorMonad
applyIntFunction f ident = do
    state <- get
    let value = getValueFromEvaluatorState ident state 
    let newValue = applyFunctionOnIntType f value
    modify $ updateValueInEvaluatorState ident newValue
    pure MockValue

applyFunctionIntOnTwoExpr :: (Integer -> Integer -> Integer) -> ValueType -> ValueType -> ValueType
applyFunctionIntOnTwoExpr f (IntValue i1) (IntValue i2) = IntValue (f i1 i2)

applyFunctionStringOnTwoExpr :: (String -> String -> String) -> ValueType -> ValueType -> ValueType
applyFunctionStringOnTwoExpr f (StringValue s1) (StringValue s2) = StringValue (f s1 s2)

evalIntegerExpr :: Evaluator a => (Integer -> Integer -> Integer) -> a -> a -> EvaluatorMonad
evalIntegerExpr f expr1 expr2 = do
    exprVal1 <- eval expr1
    exprVal2 <- eval expr2
    pure $ applyFunctionIntOnTwoExpr f exprVal1 exprVal2

evalStringExpr :: Evaluator a => (String -> String -> String) -> a -> a -> EvaluatorMonad
evalStringExpr f expr1 expr2 = do
    exprVal1 <- eval expr1
    exprVal2 <- eval expr2
    pure $ applyFunctionStringOnTwoExpr f exprVal1 exprVal2

evalComparator :: Evaluator a => (ValueType -> ValueType -> Bool) -> a -> a -> EvaluatorMonad
evalComparator f expr1 expr2 = do
    exprVal1 <- eval expr1
    exprVal2 <- eval expr2
    pure $ BoolValue $ f exprVal1 exprVal2

evalBuildInOrRegularFun :: Evaluator a => Ident -> [a] -> EvaluatorMonad -> EvaluatorMonad
evalBuildInOrRegularFun ident args exec = do
    argsVal <- mapM eval args
    if isBuildInFunction ident then evalBuildInFunction ident argsVal else exec

getArgLoc :: Expr -> EvaluatorMonad' Loc
getArgLoc (EVar _ name) = gets $ getLocEvaluatorState name
getArgLoc _             = pure (-1)

putArgs :: (Arg, ValueType, Loc) -> EvaluatorMonad
putArgs (ValArg _ _ name, value, loc) = do
    modify $ insertNewValueInEvaluatorState name value
    pure MockValue

putArgs (VarArg _ _ name, _, loc) = do
    modify $ insertLocEvaluatorState name loc
    pure MockValue

buildInFunctionsList = ["printInt", "printBool", "printString"]

isBuildInFunction :: Ident -> Bool
isBuildInFunction (Ident name) = elem name buildInFunctionsList

evalBuildInFunction :: Ident -> [ValueType] -> EvaluatorMonad
evalBuildInFunction _ [value] = do
  liftIO $ putStrLn (showValueType value)
  pure MockValue
