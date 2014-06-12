module Data.Token where

data ProgToken = ProgT [FormToken] deriving (Eq,Show)
data FormToken = FormT IdS ExpToken    deriving (Eq,Show)

data ExpToken = FuncT IdS [ExpToken]
              | ArrayT [ExpToken]
              | ObjT [(IdS,ExpToken)]
              | VarT IdS
              | StrT String
              | NumT Double
              | BoolT Bool
              | NullT
                deriving (Eq,Show)

type IntegerS = String
type IdS      = String