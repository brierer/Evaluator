module Data.Eval
( EvalError(..)
, State
, Table
, Eval
, Type (..)
, ExpObj(..)
, EvalFunc
, FuncEntry
, TypeValidator(..)
, Func(..)
) where

import qualified Data.Map as M (Map)

import Control.Monad.State     (StateT)
import Data.Token              (ExpToken,Pos)

data EvalError = MultipleDefinitions Pos String
               | UndefinedVariable Pos String
               | CycleInDefinitions [(Pos,String)]
               | UndefinedFunction Pos String
               | NonTopLevelShow Pos
               | NoShow
               | InvalidNbOfArgs Pos String Int Int
               | ArgError Int String EvalError
               | TypeMismatch Pos Type Type 
               | IllegalEmpty Pos
               | TableHeaderLengthMismatch Pos Int Int
                 deriving (Eq,Show)


type State = (Table,Table)
type Table = M.Map String (ExpToken,Pos)
type Eval = Either EvalError

data Type = Table | Plot | FunCall | Arr | Obj | Str | Num | Bool | Null
          | Type `Or` Type
            deriving (Eq,Show)

data ExpObj = TableO  Pos [[ExpObj]]        ExpObj -- Pos -> Cols  -> Opts
            | PlotO   Pos [(ExpObj,ExpObj)] ExpObj -- Pos -> Pairs -> Opts 
            | ArrayO  Pos [ExpObj]
            | ObjO    Pos [(String,ExpObj)]
            | StrO    Pos String
            | NumO    Pos Double
            | BoolO   Pos Bool
            | NullO   Pos
              deriving (Eq,Show)

type EvalFunc = StateT [FuncEntry] Eval
type FuncEntry = (String,([TypeValidator],Func))

newtype TypeValidator = TypeVal { runValidation :: ExpObj -> EvalFunc ExpObj }
newtype Func = Func (Pos -> [ExpObj] -> EvalFunc ExpObj)








