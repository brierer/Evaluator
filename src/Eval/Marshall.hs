module Eval.Marshall
( marshallWith
, marshall
) where

import Prelude hiding (any)

import qualified Prelude  as P

import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.ExpToken
import Eval.MatchType

marshallWith :: ExpToken -> [FuncEntry] -> Eval ExpObj
marshallWith x = evalStateT $ marshall x

marshall :: ExpToken -> EvalFunc ExpObj
marshall (FuncT _ (IdT p _ i) es) = applyFunc p i es
marshall (ArrT p _ es)            = liftM (ArrO p) $ mapM marshall es
marshall (ObjT p _ ps)            = liftM (ObjO p) $ mapM f        ps where f (PairT (IdT _ _ x) y) = liftM2 (,) (return x) (marshall y)
marshall (StrT p _ s)             = return $ StrO p s
marshall (NumT p _ _ n)           = return $ NumO p n
marshall (BoolT p _ b)            = return $ BoolO p b
marshall (NullT p _)              = return $ NullO p
marshall e                        = error $ "Eval.Marshall::marshall [Unexpected pattern ["++show e++"]]"

applyFunc :: Pos -> String -> [ExpToken] -> EvalFunc ExpObj
applyFunc p i es = do fs <- get; case lookup' fs of {
  Nothing -> error $ "Eval.Marshall::applyFunc [Could not lookup function ["++i++"]]";
  Just (ts,func) -> validateArgCount i (length ts) (length es) >> mapM marshall es >>= zipWithM ($) (map matchType ts) >>= invoke func p }
--
  where lookup' = lookup i .map (\(x,y,z)->(x,(y,z)))
        validateArgCount s l1 l2 = when (l1 /= l2) $ lift $ Left $ ArgCountMismatch p s l1 l2













