{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Data.Type where

import GHC.Generics
import Data.Aeson

data Type = Null | Bool | Num | Str | ObjOf [(String,Type)] | ArrOf Type | Plot | Table | Widget
          | Or [Type] 
            deriving (Eq,Ord,Show, Generic)

data TypeHead = LeafNull | LeafBool | LeafNum | LeafStr
              | NodeObj  | NodeArr
              | LeafPlot | LeafTable | LeafWidget | NodeOr [TypeHead]
                deriving (Eq,Ord,Show, Generic)


instance ToJSON TypeHead
instance ToJSON Type