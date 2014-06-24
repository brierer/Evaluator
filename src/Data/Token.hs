module Data.Token where

data ProgToken = ProgT [FormToken]  deriving (Eq,Show)
data FormToken = FormT IdToken ExpToken deriving (Eq,Show)
data PairToken = PairT IdToken ExpToken deriving (Eq,Show)
data IdToken   = IdT W2 String      deriving (Eq,Show)

data ExpToken = FuncT  W1 IdToken [ExpToken]
              | ArrayT W2 [ExpToken]
              | ObjT   W2 [PairToken]
              | VarT      IdToken
              | StrT   W2 String
              | NumT   W2 String Double
              | BoolT  W2 Bool
              | NullT  W2
                deriving (Eq,Show)

type IntegerS = String

type W1 =  String
type W2 = (String,String)
