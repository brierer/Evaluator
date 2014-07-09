module Data.Eval
( State
, Table
, Eval
, EvalFunc
, FuncEntry
, TypeValidator(..)
, Func(..)
) where

import qualified Data.Map as M (Map)

import Control.Monad.State     (StateT)
import Data.EvalError          (EvalError)
import Data.ExpToken           (ExpToken,Pos)
import Data.ExpObj             (ExpObj)

type State = (Table,Table)
type Table = M.Map String (ExpToken,Pos)
type Eval = Either EvalError

type EvalFunc = StateT [FuncEntry] Eval
type FuncEntry = (String,([TypeValidator],Func))

newtype TypeValidator = TypeVal { runValidation :: ExpObj -> EvalFunc ExpObj }
newtype Func = Func (Pos -> [ExpObj] -> EvalFunc ExpObj)








