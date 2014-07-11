module Data.ExpToken
( ProgToken(..)
, FormToken(..)
, PairToken(..)
, IdToken(..)
, ExpToken(..)
, Pos,IdS,IntegerS,W1,W2
) where

data ProgToken = ProgT Pos [FormToken]       deriving (Eq,Show)
data FormToken = FormT      IdToken ExpToken deriving (Eq,Show)
data PairToken = PairT      IdToken ExpToken deriving (Eq,Show)
data IdToken   = IdT   Pos  W2 String        deriving (Eq,Show)

data ExpToken = FuncT     W1  IdToken [ExpToken]
              | ArrT  Pos W2 [ExpToken]
              | ObjT  Pos W2 [PairToken]
              | VarT          IdToken
              | StrT  Pos W2  String
              | NumT  Pos W2  String Double
              | BoolT Pos W2  Bool
              | NullT Pos W2
                deriving (Eq,Show)

type Pos      = (Int,Int)
type IdS      = String
type IntegerS = String

type W1 =  String
type W2 = (String,String)
