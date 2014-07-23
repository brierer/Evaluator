module Data.HasPos where

import Data.ExpToken
import Data.ExpObj

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