{-# LANGUAGE RecordWildCards #-}

module EvaluatorEnv where

import           AbsWyso
import           Prelude
import           EvaluatorTypes

import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- | Environment and state definitions

data EvaluatorState = EvaluatorState
    { env :: Env
    , store :: Store
    }

data Store = Store
    { storeInternal :: Map.Map Loc ValueType
    , lastLoc :: Loc
    }

instance Show Store where
    show Store{..} = "Store\n " ++ show storeInternal

instance Show EvaluatorState where
    show EvaluatorState{..} = "EvaluatorState " ++ show env ++ "\nStore czas\n" ++ show store ++ "\nXXDXDXDDDXD\n"

instance Show Env where
    show Env{..} = "Env " ++ show envInternal

emptyEnv = Env
    { envInternal = Map.empty
    }

emptyStore = Store
    { storeInternal = Map.empty
    , lastLoc = 0
    }

emptyEvaluatorState = EvaluatorState
    { env = emptyEnv
    , store = emptyStore
    }

mock_loc = -1

getEnvLoc :: Ident -> Env -> Loc
getEnvLoc ident Env{..} =
    case Map.lookup ident envInternal of
        Just loc -> loc
        Nothing -> mock_loc

insertEnvLoc :: Ident -> Loc -> Env -> Env
insertEnvLoc ident loc Env{..} = Env
    { envInternal = Map.insert ident loc envInternal
    }

getStoredValue :: Loc -> Store -> ValueType
getStoredValue loc Store{..} = fromJust $ Map.lookup loc storeInternal

updateStoredValue :: Loc -> ValueType -> Store -> Store
updateStoredValue loc value Store{..} = Store
    { storeInternal = Map.insert loc value storeInternal
    , lastLoc = lastLoc
    }

insertNewStoredValue :: ValueType -> Store -> (Loc, Store)
insertNewStoredValue value Store{..} = (lastLoc + 1, Store
    { storeInternal = Map.insert (lastLoc + 1) value storeInternal
    , lastLoc = lastLoc + 1
    })

getValueFromEvaluatorState :: Ident -> EvaluatorState -> ValueType
getValueFromEvaluatorState ident EvaluatorState{..} = getStoredValue (getEnvLoc ident env) store

updateValueInEvaluatorState :: Ident -> ValueType -> EvaluatorState -> EvaluatorState
updateValueInEvaluatorState ident value EvaluatorState{..} = EvaluatorState
    { env = env
    , store = updateStoredValue (getEnvLoc ident env) value store
    }

insertNewValueInEvaluatorState :: Ident -> ValueType -> EvaluatorState -> EvaluatorState
insertNewValueInEvaluatorState ident value EvaluatorState{..} = let (loc, newStore) = insertNewStoredValue value store in EvaluatorState
    { env = insertEnvLoc ident loc env
    , store = newStore
    }

getLocEvaluatorState :: Ident -> EvaluatorState -> Loc
getLocEvaluatorState ident EvaluatorState{..} = getEnvLoc ident env

insertLocEvaluatorState :: Ident -> Loc -> EvaluatorState -> EvaluatorState
insertLocEvaluatorState ident loc EvaluatorState{..} = EvaluatorState
    { env = insertEnvLoc ident loc env
    , store = store
    }

getEnvEvaluatorState :: EvaluatorState -> Env
getEnvEvaluatorState EvaluatorState{..} = env

insertEnvEvaluatorState :: Env -> EvaluatorState -> EvaluatorState
insertEnvEvaluatorState newEnv EvaluatorState{..} = EvaluatorState
    { env = newEnv
    , store = store
    }

returnLabel = Ident "return"

insertDefaultReturnValue :: EvaluatorState -> EvaluatorState
insertDefaultReturnValue state = insertNewValueInEvaluatorState returnLabel MockValue state

updateReturnValue :: ValueType -> EvaluatorState -> EvaluatorState
updateReturnValue value state = updateValueInEvaluatorState returnLabel value state

getReturnValue :: EvaluatorState -> ValueType
getReturnValue state = getValueFromEvaluatorState returnLabel state

isReturnNotDefined :: EvaluatorState -> Bool
isReturnNotDefined state = isMock (getReturnValue state)

getFunctionArgs :: Ident -> EvaluatorState -> [Arg]
getFunctionArgs ident state = case getValueFromEvaluatorState ident state of
    FunValue args _ _ -> args
    _ -> []

getFunctionBlock :: Ident -> EvaluatorState -> Block
getFunctionBlock ident state = case getValueFromEvaluatorState ident state of
    FunValue _ block _ -> block
    
getFunctionEnv :: Ident -> EvaluatorState -> Env
getFunctionEnv ident state = case getValueFromEvaluatorState ident state of
    FunValue _ _ env -> env
    _ -> emptyEnv

insertEvaluatorStateValue :: Ident -> ValueType -> EvaluatorState -> EvaluatorState
insertEvaluatorStateValue name value EvaluatorState{..} =
    let
        (newLoc, newStore) = insertStoreValue  value store
        newEnv = insertEnvloc   name newLoc env
    in
        EvaluatorState { env = newEnv, store = newStore }

insertStoreValue  :: ValueType -> Store -> (Loc, Store)
insertStoreValue  value Store{..} =
    let
        newLoc = lastLoc + 1
        newStore = Map.insert newLoc value storeInternal
    in
        (newLoc, Store { storeInternal = newStore, lastLoc = newLoc })

insertEnvloc   :: Ident -> Loc -> Env -> Env
insertEnvloc   name loc Env{..} =
  Env{envInternal=Map.insert name loc envInternal}


addForRecursionValueInEvaluatorState :: Ident -> ValueType -> EvaluatorState -> EvaluatorState
addForRecursionValueInEvaluatorState name value EvaluatorState{..} =
    let
        (newloc, newStore) = insertNewStoredValue value store
        newEnv = insertEnvLoc name newloc env
    in
        EvaluatorState { env = newEnv, store = newStore }