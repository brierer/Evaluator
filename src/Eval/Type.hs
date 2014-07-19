module Eval.Type
( HasTypeRoot(..)
, any,none,arr,obj
, matchType
) where

import Prelude hiding (any)

import qualified Prelude  as P
import qualified Data.Set as S

import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.HasPos

class HasTypeRoot a where getRoot :: a -> TypeRoot
instance HasTypeRoot Type where
  getRoot (ArrOf _) = NodeArr
  getRoot (ObjOf _) = NodeObj
  getRoot (Or ts)   = NodeOr $ reverse $ S.toList $ S.fromList $ map getRoot ts
  getRoot t         = Leaf t

instance HasTypeRoot ExpObj where
  getRoot (TableO{}) = Leaf Table
  getRoot (PlotO{})  = Leaf Plot
  getRoot (ArrO{})   = NodeArr
  getRoot (ObjO{})   = NodeObj
  getRoot (StrO{})   = Leaf Str
  getRoot (NumO{})   = Leaf Num
  getRoot (BoolO{})  = Leaf Bool
  getRoot (NullO{})  = Leaf Null

any  :: Type
none :: Type
arr  :: Type
obj  :: Type

any  = Or [Table,Plot,arr,obj,Str,Num,Bool,Null]
none = Or []
arr  = ArrOf any
obj  = ObjOf any

matchType :: Type -> ExpObj -> EvalFunc ExpObj
matchType  Table    x@(TableO{}) = return x
matchType  Plot     x@(PlotO{})  = return x
matchType (ArrOf t) (ArrO p es)  = liftM (ArrO p) $ mapM (matchType t) es
matchType (ObjOf t) (ObjO p ps)  = liftM (ObjO p) $ mapM (\(x,y)->liftM2 (,) (return x) $ matchType t y) ps
matchType  Str      x@(StrO{})   = return x
matchType  Num      x@(NumO{})   = return x
matchType  Bool     x@(BoolO{})  = return x
matchType  Null     x@(NullO{})  = return x
matchType t@(Or ts) x            = do fs <- get; let rs = map (flip evalStateT fs.(`matchType`x)) ts in if P.any isRight rs then return x else typeMismatch t x
matchType t         x            = typeMismatch t x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

typeMismatch :: Type -> ExpObj -> EvalFunc ExpObj
typeMismatch t x = lift $ Left $ TypeMismatch (getPos x) (getRoot t) (getRoot x)


