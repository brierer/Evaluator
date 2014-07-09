module Data.ExpObj
( Type(..)
, ExpObj(..)
) where

import Data.ExpToken (Pos)

data Type = Table | Plot | FunCall | Arr | Obj | Str | Num | Bool | Null
          | Type `Or` Type
            deriving (Eq,Show)

data ExpObj = TableO  Pos [[ExpObj]]        [ExpObj]
            | PlotO   Pos [(ExpObj,ExpObj)] [(String,ExpObj)]
            | ArrayO  Pos [ExpObj]
            | ObjO    Pos [(String,ExpObj)]
            | StrO    Pos String
            | NumO    Pos Double
            | BoolO   Pos Bool
            | NullO   Pos
              deriving (Eq,Ord,Show)