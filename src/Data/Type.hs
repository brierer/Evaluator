module Data.Type where

data Type = Null | Bool | Num | Str | ObjOf Type | ArrOf Type | Plot | Table 
          | Or [Type]
            deriving (Eq,Ord,Show)
            
data TypeTree = LeafNull | LeafBool | LeafNum | LeafStr 
              | NodeObj  | NodeArr 
              | LeafPlot | LeafTable | NodeOr [TypeTree]
                deriving (Eq,Ord,Show)
