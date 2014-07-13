module Data.ExpObj
( Type(..)
, TypeRoot(..)
, ExpObj(..)
) where

import Data.ExpToken (Pos)

data Type = Null | Bool | Num | Str | ObjOf Type | ArrOf Type | Plot | Table 
          | Or [Type]
            deriving (Eq,Ord,Show)
            
data TypeRoot = NodeArr | NodeObj | Leaf Type deriving (Eq,Ord,Show)

data ExpObj = TableO Pos [[ExpObj]]         [ExpObj]
            | PlotO  Pos [(ExpObj,ExpObj)] [(String,ExpObj)]
            | ArrO   Pos  [ExpObj]
            | ObjO   Pos [(String,ExpObj)]
            | StrO   Pos   String
            | NumO   Pos   Double
            | BoolO  Pos   Bool
            | NullO  Pos  
              deriving (Eq,Ord,Show)