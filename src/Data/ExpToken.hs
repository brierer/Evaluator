{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Data.ExpToken
( ProgToken(..)
, FormToken(..)
, PairToken(..)
, IdToken(..)
, ExpToken(..)
, Pos,IdS,IntegerS,W1,W2
, stringifyToken,
stringifyProgToken
) where

import GHC.Generics
import Data.Aeson
import Data.Text (pack)

data ProgToken = ProgT Pos [FormToken]       deriving (Eq,Show,Generic)
data FormToken = FormT      IdToken ExpToken deriving (Eq,Show,Generic)
data PairToken = PairT      IdToken ExpToken deriving (Eq,Show,Generic)
data IdToken   = IdT   Pos  W2 String        deriving (Eq,Show,Generic)

data ExpToken = FuncT     W1  IdToken [ExpToken]
              | ArrT  Pos W2 [ExpToken]
              | ObjT  Pos W2 [PairToken]
              | VarT          IdToken
              | StrT  Pos W2  String
              | NumT  Pos W2  String Double
              | BoolT Pos W2  Bool
              | NullT Pos W2
                deriving (Eq, Show, Generic)

instance ToJSON ExpToken
instance ToJSON ProgToken
instance ToJSON FormToken
instance ToJSON IdToken
instance ToJSON PairToken where
	toJSON (PairT (IdT p w s) exp) = object [ (pack s) .= exp] 

type Pos      = (Int,Int)
type IdS      = String
type IntegerS = String

type W1 =  String
type W2 = (String,String)


stringifyProgToken :: ProgToken -> String
stringifyProgToken (ProgT p ds) = foldBetween (map stringifyForm ds) ['\n']

stringifyForm :: FormToken -> String
stringifyForm (FormT i e) =  (stringifyFunc i) ++ " = " ++ (stringifyToken e) 

stringifyToken :: ExpToken -> String
stringifyToken (FuncT w1 id es) = w1 ++ (stringifyFunc id) ++ "(" ++  (foldBetween (map stringifyToken es) [',']) ++ ")"
stringifyToken (ArrT p (w1,w2) es) = "[" ++ w1 ++ (foldBetween (map stringifyToken es) [',']) ++ w2 ++ "]"
stringifyToken (ObjT p (w1,w2) ps) = "{" ++ w1 ++ (foldBetween (map stringifyPair  ps) [',']) ++  w2 ++ "}"
stringifyToken (VarT i) = stringifyFunc i 
stringifyToken (StrT p (w1,w2) s) = show $ s
stringifyToken (NumT p (w1,w2) s d) = s
stringifyToken (BoolT p (w1,w2) b) = show b
stringifyToken (NullT p w2) = "null"

stringifyFunc :: IdToken -> String
stringifyFunc (IdT p w s) = s

stringifyPair :: PairToken -> String
stringifyPair (PairT i e) = (stringifyFunc i)  ++ ":" ++ (stringifyToken e)
 
foldBetween :: [String] -> String -> String
foldBetween ([]) item = ""
foldBetween (d:[]) item = d
foldBetween (d:ds) item =  foldl (\x y -> x ++ item ++ y) (d) (ds)
