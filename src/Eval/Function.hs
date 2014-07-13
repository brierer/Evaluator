module Eval.Function
( HasPos(..),HasTypeRoot(..)
, any,none,arr,obj
, marshall
, matchType
) where

import Prelude hiding (any)

import qualified Prelude  as P

import Control.Monad.State
import Data.Eval   
import Data.EvalError           
import Data.ExpObj            
import Data.ExpToken          

class HasPos a where getPos   :: a -> Pos
--
--instance HasPos ExpToken where
--  getPos (ArrT p _ _)   = p
--  getPos (ObjT p _ _)   = p
--  getPos (StrT p _ _)   = p
--  getPos (NumT p _ _ _) = p
--  getPos (BoolT p _ _)  = p
--  getPos (NullT p _)    = p
--  getPos e              = error $ "Eval.Function::getPos<ExpToken> [Unexpected pattern ["++show e++"]]"
--
instance HasPos ExpObj where
  getPos (TableO p _ _) = p
  getPos (PlotO  p _ _) = p
  getPos (ArrO p _)     = p
  getPos (ObjO p _)     = p
  getPos (StrO p _)     = p
  getPos (NumO p _)     = p
  getPos (BoolO p _)    = p
  getPos (NullO p)      = p

class HasTypeRoot a where getRoot :: a -> TypeRoot
instance HasTypeRoot Type where
  getRoot (ArrOf _) = NodeArr
  getRoot (ObjOf _) = NodeObj
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

any  = Or [Table,Plot,arr,obj,Str,Num,Bool,Null]
none = Or []
arr  = ArrOf any
obj  = ObjOf any

matchType (ArrOf t) (ArrO p es)  = liftM (ArrO p) $ mapM (matchType t) es
matchType (ObjOf t) (ObjO p ps)  = liftM (ObjO p) $ mapM (\(x,y)->liftM2 (,) (return x) $ matchType t y) ps
matchType  Str      x@(StrO{})   = return x
matchType  Num      x@(NumO{})   = return x
matchType  Bool     x@(BoolO{})  = return x
matchType  Null     x@(NullO{})  = return x   
matchType  t        x            = evalError $ TypeMismatch (getPos x) (getRoot t) (getRoot x)

marshall :: ExpToken -> EvalFunc ExpObj
marshall (FuncT _ (IdT p _ i) es) = applyFunc p i es
--marshall (ArrT p _ es)            = liftM (ArrO p) $ mapM marshall es
--marshall (ObjT p _ ps)            = liftM (ObjO p) $ mapM f        ps where f (PairT (IdT _ _ x) y) = liftM2 (,) (return x) (marshall y)
marshall (StrT p _ s)             = return $ StrO p s
marshall (NumT p _ _ n)           = return $ NumO p n
marshall (BoolT p _ b)            = return $ BoolO p b
marshall (NullT p _)              = return $ NullO p
--marshall e                        = error $ "Eval.Function::marshall [Unexpected pattern ["++show e++"]]"

applyFunc p i es = 
  do fs <- get; 
     case lookup' i fs of 
       Nothing -> error $ "Function::applyFunc [Could not lookup function ["++i++"]]"
       Just (ts,Func _) -> do
         validateArgCount p i (length ts) (length es)
         _ <- zipWithM ($) (map matchType ts) =<< mapM marshall es
         error "Function::applyFunc [Function not implemented beyong arg validation]"
      
lookup' i = lookup i .map (\(x,y,z)->(x,(y,z)))       
validateArgCount p s l1 l2 = when (l1 /= l2) $ evalError $ ArgCountMismatch p s l1 l2
evalError = lift.Left


















