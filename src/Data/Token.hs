module Data.Token where

data ProgToken = ProgT [FormToken]  deriving (Eq,Show)
data FormToken = FormT IdS ExpToken deriving (Eq,Show)
data PairToken = PairT IdS ExpToken deriving (Eq,Show)

data ExpToken = FuncT  W1 IdS [ExpToken]
              | ArrayT W2 [ExpToken]
              | ObjT   W2 [PairToken]
              | VarT   W2 IdS
              | StrT   W2 String
              | NumT   W2 String Double
              | BoolT  W2 Bool
              | NullT  W2
                deriving (Eq,Show)

type IntegerS = String
type IdS      = String

type W1 =  String
type W2 = (String,String)
