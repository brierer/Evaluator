module Eval.MatchType
( getRoot
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
import Data.Type

class HasType a where getRoot :: a -> TypeTree
instance HasType Type where
  getRoot  Table    = LeafTable
  getRoot  Plot     = LeafPlot
  getRoot  Str      = LeafStr
  getRoot  Num      = LeafNum
  getRoot  Bool     = LeafBool
  getRoot  Null     = LeafNull
  getRoot (ArrOf _) = NodeArr
  getRoot (ObjOf _) = NodeObj
  getRoot (Or ts)   = NodeOr $ reverse $ S.toList $ S.fromList $ map getRoot ts

instance HasType ExpObj where
  getRoot (TableO{}) = LeafTable
  getRoot (PlotO{})  = LeafPlot
  getRoot (StrO{})   = LeafStr
  getRoot (NumO{})   = LeafNum
  getRoot (BoolO{})  = LeafBool
  getRoot (NullO{})  = LeafNull

  getRoot (ArrO{})   = NodeArr
  getRoot (ObjO{})   = NodeObj

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
matchType (Or ts)   x            = do fs <- get; let rs = map (flip evalStateT fs.(`matchType`x)) ts in if P.any isRight rs then return x else orTypeMismatch rs 
matchType t         x            = typeMismatch t x

orTypeMismatch ::  [Eval ExpObj] -> EvalFunc a
orTypeMismatch = lift . Left . TypeMismatch . TMNode . map f where
  f (Left (TypeMismatch t)) = t
  f x                       = error $ "Eval.MatchTYpe::orType::f [Unexpected pattern ["++show x++"]]"  

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

typeMismatch :: Type -> ExpObj -> EvalFunc a
typeMismatch t x = lift $ Left $ TypeMismatch $ TMLeaf (getPos x) (getRoot t) (getRoot x)


