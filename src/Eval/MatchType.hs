module Eval.MatchType
( getRoot
, any,none,arr
, matchType
) where

import Prelude hiding (any)

import qualified Prelude  as P
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.Type

class HasType a where getRoot :: a -> TypeHead
instance HasType Type where
  getRoot  Table    = LeafTable
  getRoot  Plot     = LeafPlot
  getRoot  Widget   = LeafWidget
  getRoot  Str      = LeafStr
  getRoot  Num      = LeafNum
  getRoot  Bool     = LeafBool
  getRoot  Null     = LeafNull
  getRoot (ArrOf _) = NodeArr
  getRoot (ObjOf _) = NodeObj
  getRoot (Or ts)   = NodeOr $ sortNub $ map getRoot ts

instance HasType ExpObj where
  getRoot (TableO{}) = LeafTable
  getRoot (PlotO{})  = LeafPlot
  getRoot (WidgetO{})= LeafWidget
  getRoot (StrO{})   = LeafStr
  getRoot (NumO{})   = LeafNum
  getRoot (BoolO{})  = LeafBool
  getRoot (NullO{})  = LeafNull

  getRoot (ArrO{})   = NodeArr
  getRoot (ObjO{})   = NodeObj

any  :: Type
none :: Type
arr  :: Type
--obj  :: Type

any  = Or [Table,Plot,arr,Str,Num,Bool,Null]
none = Or []
arr  = ArrOf any
--obj  = ObjOf any

matchType :: Type -> ExpObj -> EvalFunc ExpObj
matchType  Table    x@(TableO{})   = return x
matchType  Plot     x@(PlotO{})    = return x
matchType  Widget   x@(WidgetO{})  = return x
matchType (ArrOf t)   (ArrO p es)  = liftM (ArrO p) $ mapM (matchType t) es
matchType (ObjOf t)   (ObjO p ps)  = liftM (ObjO p) $ mapM (\(s,(o,t))->liftM2 (,) (return s) $ matchType t o) (objByTag t ps)
matchType  Str      x@(StrO{})     = return x
matchType  Num      x@(NumO{})     = return x
matchType  Bool     x@(BoolO{})    = return x
matchType  Null     x@(NullO{})    = return x
matchType (Or ts)   x              = do fs <- get; let rs = map (flip evalStateT fs.(`matchType`x)) ts in if P.any isRight rs then return x else orTypeMismatch x rs
matchType t         x              = typeMismatch t x

getObjByTag :: String -> [(String, b)] -> Maybe b
getObjByTag s ps = M.lookup s (M.fromList ps)

objByTag  :: [(String, Type)] -> [(String, ExpObj)] -> [(String,(ExpObj, Type))]
objByTag  t [] = []
objByTag  t ((s,o):os) = case (getObjByTag s t) of 
                            Just x -> [(s,(o,x))] ++ objByTag t os  
                            Nothing -> [] ++ objByTag t os  


orTypeMismatch ::  ExpObj -> [Eval ExpObj] -> EvalFunc a
orTypeMismatch x = simplify x . map f where
  f (Left (TypeMismatch t)) = t
  f e                       = error $ "Eval.MatchType::orType::f [Unexpected pattern ["++show e++"]]"

simplify :: ExpObj -> [TMTree] -> EvalFunc a
simplify x [] = typeMismatch (Or []) x
simplify _ ts | P.any (not.isLeaf) ts = typeError $ TMNode ts
              | otherwise = let (canBeSame,orTypes) = unzip $ map (\(TMLeaf x y z) -> ((x,z),y)) ts
                                (p,t2) = head canBeSame
                                canBeSimplified = length (S.toList $ S.fromList canBeSame) == 1
                            in typeError $ if canBeSimplified then TMLeaf p (NodeOr $ sortNub orTypes) t2 else TMNode ts

sortNub :: [TypeHead] -> [TypeHead]
sortNub = reverse . S.toList . S.fromList

isLeaf :: TMTree -> Bool
isLeaf (TMLeaf{}) = True
isLeaf _          = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

typeMismatch :: Type -> ExpObj -> EvalFunc a
typeMismatch t x = typeError $ TMLeaf (getPos x) (getRoot t) (getRoot x)

typeError :: TMTree -> EvalFunc a
typeError = lift .Left .TypeMismatch

