{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}


module Data.EvalResult (
	EvalResult(..),
	TableSTR(..),
	) where

import Data.Aeson
import Data.Typeable
import GHC.Generics
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.ExpToken

data EvalResult = 
	OK {eq :: TableSTR, res :: (ExpObj) }
	| TK EvalError 
	| KO {eq :: TableSTR, err :: EvalError} 
	deriving (Show,Typeable, Generic)
data TableSTR = OK_Table String | KO_Table EvalError deriving (Show, Generic)

instance ToJSON EvalResult
instance ToJSON TableSTR
