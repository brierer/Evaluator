{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Data.EvalError
( EvalError(..)
, TMTree(..)
) where

import Data.ExpToken
import Data.Type
import GHC.Generics
import Data.Aeson

data EvalError = InvalidParse Pos [String]
               | MultipleDefinitions Pos String
               | UndefinedVariable Pos String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | ArgCountMismatch Pos String Int Int
               | TypeMismatch TMTree
               | IllegalEmpty Pos
               | TableColumnLengthMismatch Pos Int Int
               | TableHeaderLengthMismatch Pos Int Int
               | IllegalTakeTableLength Pos Int Int
               | IndexOutOfBounds Pos Int Int Int
                 deriving (Eq,Show, Generic)


data TMTree = TMLeaf { _pos :: Pos , _exp :: TypeHead , _act :: TypeHead }
            | TMNode [TMTree]
              deriving (Eq,Show, Generic)

instance ToJSON EvalError
instance ToJSON TMTree

