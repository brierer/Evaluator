module Data.Token where

data ProgToken = ProgT Pos [FormToken]  deriving (Show)
data FormToken = FormT Pos IdToken ExpToken deriving (Show)
data PairToken = PairT Pos IdToken ExpToken deriving (Show)
data IdToken   = IdT   Pos W2 String      deriving (Show)

data ExpToken = FuncT  Pos W1 IdToken [ExpToken]
              | ArrayT Pos W2 [ExpToken]
              | ObjT   Pos W2 [PairToken]
              | VarT   Pos    IdToken
              | StrT   Pos W2 String
              | NumT   Pos W2 String Double
              | BoolT  Pos W2 Bool
              | NullT  Pos W2
                deriving (Show)

{-| Ignore position in Eq instance -}
instance Eq ProgToken where (ProgT _ a) == (ProgT _ b) = a == b
instance Eq FormToken where (FormT _ a1 b1) == (FormT _ a2 b2) = (a1,b1) == (a2,b2)
instance Eq PairToken where (PairT _ a1 b1) == (PairT _ a2 b2) = (a1,b1) == (a2,b2)
instance Eq IdToken   where (IdT _ a1 b1)   == (IdT _ a2 b2)   = (a1,b1) == (a2,b2)
instance Eq ExpToken where
  (FuncT _  a1 b1 c1) == (FuncT _  a2 b2 c2) = (a1,b1,c1) == (a2,b2,c2)
  (ArrayT _ a1 b1)    == (ArrayT _ a2 b2)    = (a1,b1)    == (a2,b2)
  (ObjT _   a1 b1)    == (ObjT _   a2 b2)    = (a1,b1)    == (a2,b2)
  (VarT _   a1)       == (VarT _   a2)       =  a1        ==  a2
  (StrT _   a1 b1)    == (StrT _   a2 b2)    = (a1,b1)    == (a2,b2)
  (NumT _   a1 b1 c1) == (NumT _   a2 b2 c2) = (a1,b1,c1) == (a2,b2,c2)
  (BoolT _  a1 b1)    == (BoolT _  a2 b2)    = (a1,b1)    == (a2,b2)
  (NullT _  a1)       == (NullT _  a2)       =  a1        ==  a2
  _                   == _                   = False
  
type Pos = (Int,Int)
type IntegerS = String

type W1 =  String
type W2 = (String,String)
