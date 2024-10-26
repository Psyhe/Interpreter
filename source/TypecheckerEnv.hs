{-# LANGUAGE RecordWildCards #-}

module TypecheckerEnv where

import           AbsWyso
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Prelude
import           Exceptions
import           TypecheckerTypes

import qualified Data.Map as Map

-- | Names are unique for both variables and functions.
data Env = Env
  { identifiers :: Map.Map Ident PureType
  , didReturn :: Bool
  }

instance Show Env where
  show Env{..} = "Env " ++ show identifiers ++ " didReturn: " ++ show didReturn

emptyEnv = Env
 {
    identifiers = buildInFunctions
    , didReturn = False
 }

buildInFunctions :: Map.Map Ident PureType
buildInFunctions = Map.fromList
  [ (Ident "printInt", FunType [IntType] VoidType)
  , (Ident "printString", FunType [StringType] VoidType)
  , (Ident "printBool", FunType [BoolType] VoidType)
  ]

updateType :: Ident -> PureType -> Env -> Env
updateType ident pureType  Env{..} = Env
  { identifiers = Map.insert ident pureType identifiers
  , didReturn = didReturn
  }

getIdentifiers :: Env -> Map.Map Ident PureType
getIdentifiers env = identifiers env

updateEnv newIdentifiers newDidReturn env = Env
  { identifiers = newIdentifiers
  , didReturn = newDidReturn
  }

updateTypePair :: (Ident, PureType) -> Env -> Env
updateTypePair (ident, pureType) env = updateType ident pureType env

updateTypeList :: [(Ident, PureType)] -> Env -> Env
updateTypeList pairs env = foldl (flip updateTypePair) env pairs

setDidReturn :: Bool -> Env -> Env
setDidReturn didReturn env = Env
  { identifiers = identifiers env
  , didReturn = didReturn
  }

getDidReturn :: Env -> Bool
getDidReturn env = didReturn env

getPureTypeFromEnv :: Ident -> Env -> Maybe PureType
getPureTypeFromEnv name env = Map.lookup name (identifiers env)

getFunctionArgsFromEnv :: Ident -> Env -> PureType
getFunctionArgsFromEnv name env = fromJust $ Map.lookup name (identifiers env) where
  fromJust (Just x) = x
  fromJust Nothing = error "Function not found"


