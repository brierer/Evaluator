module Data.Type where

data Type = Null | Bool | Num | Str | ObjOf Type | ArrOf Type | Plot | Table
          | Or [Type]
            deriving (Eq,Ord,Show)

data TypeHead = LeafNull | LeafBool | LeafNum | LeafStr
              | NodeObj  | NodeArr
              | LeafPlot | LeafTable | NodeOr [TypeHead]
                deriving (Eq,Ord,Show)
