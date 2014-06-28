module Data.Eval
( EvalError(..)
, State
, Table
, Eval
, Type (..)
, ExpObj(..)
, FuncEntry
, TypeValidator
, Func
) where

import qualified Data.Map as M (Map)

import Data.Token              (ExpToken,Pos)

data EvalError = MultipleDefinitions Pos String
               | UndefinedVariable Pos String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | InvalidNbOfArgs Pos String Int Int
               | TypeMismatch Pos Type Type
                 deriving (Eq,Show)

type State = (Table,Table)
type Table = M.Map String (ExpToken,Pos)
type Eval = Either EvalError

data Type = Table | Plot | FunCall | Array | Object | String | Number | Boolean | Null 
          | Type `Or` Type 
            deriving (Eq,Show)

data ExpObj = TableO  Pos ExpObj ExpObj        -- Pos -> Array -> Object
            | PlotO   Pos ExpObj ExpObj ExpObj -- Pos -> Array -> Array -> Object
            | ArrayO  Pos [ExpObj]
            | ObjO    Pos [(String,ExpObj)]
            | StrO    Pos String
            | NumO    Pos Double
            | BoolO   Pos Bool
            | NullO   Pos
              deriving (Eq,Show)

type FuncEntry a = (String,([TypeValidator a],Func))
type TypeValidator a = a -> Eval ExpObj
type Func = [ExpObj] -> Eval ExpObj