module Eval.Function
( Marshallable(..)
, any,   anyType
, noLit, noLitType
, lit,   litType
, (<|>), (<!>)
, applyFunc
) where

import Prelude hiding      (any,null)

import Control.Monad       (liftM,liftM2,zipWithM,when)
import Data.Eval           (Type(..),ExpObj(..),EvalError(..),Eval,FuncEntry,TypeValidator,Func)
import Data.Token          (PairToken(..),IdToken(..),ExpToken(..),Pos)

class Marshallable a where
  arrayOf  :: TypeValidator a -> TypeValidator a
  
  table    :: [FuncEntry a] -> TypeValidator a
  plot     :: [FuncEntry a] -> TypeValidator a
  funCall  :: [FuncEntry a] -> TypeValidator a
  array    :: [FuncEntry a] -> TypeValidator a; array = arrayOf.any 
  obj      :: [FuncEntry a] -> TypeValidator a
  str      :: TypeValidator a
  num      :: TypeValidator a
  bool     :: TypeValidator a
  null     :: TypeValidator a

  getP :: a -> Pos
  getT :: a -> Type

instance Marshallable ExpToken where
  table _    = typeMismatch litType
  plot _     = typeMismatch litType
  
  arrayOf v  (ArrayT p _ es)  = liftM (ArrayO p) $ mapM v es;            arrayOf _ e = typeMismatch Array   e

  funCall fs f@(FuncT{})      = applyFunc fs f;                          funCall _ e = typeMismatch FunCall e
  obj fs     (ObjT p _ ps)    = liftM (ObjO p)   $ mapM (toTuple fs) ps; obj _ e     = typeMismatch Object  e
  str        (StrT p _ s)     = return $ StrO p s;                       str e       = typeMismatch String  e
  num        (NumT p _ _ n)   = return $ NumO p n;                       num e       = typeMismatch Number  e
  bool       (BoolT p _ b)    = return $ BoolO p b;                      bool e      = typeMismatch Boolean e
  null       (NullT p _)      = return $ NullO p;                        null e      = typeMismatch Null    e
  
  getP (FuncT _ (IdT p _ _) _) = p
  getP (ArrayT p _ _)          = p
  getP (ObjT p _ _)            = p
  getP (StrT p _ _)            = p
  getP (NumT p _ _ _)          = p
  getP (BoolT p _ _)           = p
  getP (NullT p _)             = p
  getP e                       = error $ "Eval.Function::getP<ExpToken> [Failed pattern match ["++show e++"]]"
  
  getT (ArrayT{})     = Array  
  getT (ObjT{})       = Object 
  getT (StrT{})       = String 
  getT (NumT{})       = Number 
  getT (BoolT{})      = Boolean
  getT (NullT{})      = Null   
  getT e              = error $ "8val.Function::getT<ExpToken> [Failed pattern match ["++show e++"]]"

instance Marshallable ExpObj where
  funCall _ = typeMismatch FunCall
  
  arrayOf v (ArrayO p es) = liftM (ArrayO p) $ mapM v es; arrayOf _ e = typeMismatch Array   e
  
  table _   x@(TableO{}) = return x;                      table _ e   = typeMismatch Table   e
  plot _    x@(PlotO{})  = return x;                      plot _ e    = typeMismatch Plot    e
  obj _     x@(ObjO{})   = return x;                      obj _ e     = typeMismatch Object  e
  str       x@(StrO{})   = return x;                      str e       = typeMismatch String  e
  num       x@(NumO{})   = return x;                      num e       = typeMismatch Number  e
  bool      x@(BoolO{})  = return x;                      bool e      = typeMismatch Boolean e
  null      x@(NullO{})  = return x;                      null e      = typeMismatch Null    e
  
  getP (TableO p _ _)  = p
  getP (PlotO p _ _ _) = p
  getP (ArrayO p _)    = p
  getP (ObjO p _)      = p
  getP (StrO p _)      = p
  getP (NumO p _)      = p
  getP (BoolO p _)     = p
  getP (NullO p)       = p
  
  getT (TableO{})  = Table
  getT (PlotO{})   = Plot
  getT (ArrayO{})  = Array
  getT (ObjO{})    = Object
  getT (StrO{})    = String
  getT (NumO{})    = Number
  getT (BoolO{})   = Boolean
  getT (NullO{})   = Null

any   :: Marshallable a => [FuncEntry a] -> TypeValidator a
noLit :: Marshallable a => [FuncEntry a] -> TypeValidator a
lit   :: Marshallable a => [FuncEntry a] -> TypeValidator a

any   fs = table fs <|> plot fs <|> funCall fs <|> array fs <|> obj fs <|> str <|> num <|> bool <|> null <!> anyType
noLit fs = table fs <|> plot fs                                                                          <!> noLitType
lit   fs =                          funCall fs <|> array fs <|> obj fs <|> str <|> num <|> bool <|> null <!> litType

anyType   :: Type
litType   :: Type
noLitType :: Type

anyType   = foldl1 Or [noLitType,litType]
noLitType = foldl1 Or [Table,Plot]
litType   = foldl1 Or [Array,Object,String,Number,Boolean,Null]


(<|>) :: TypeValidator a -> TypeValidator a -> TypeValidator a
infixl 3 <|>
(a <|> b) x = case a x of r@(Right _) -> r; Left _ -> b x

infixl 3 <!>
(<!>) :: Marshallable a => TypeValidator a -> Type -> TypeValidator a
(<!>) v = (<|>) v . typeMismatch

{-| Function validation and application -}
applyFunc :: [(String,([TypeValidator ExpToken],Func))] -> ExpToken -> Eval ExpObj
applyFunc fs (FuncT _ (IdT p _ i) es) = apply fs p i es
applyFunc _  e                        = error $ "MultiPass::applyFunc [Failed pattern match ["++show e++"]]"

apply :: [(String,([TypeValidator ExpToken],Func))] -> Pos -> String -> [ExpToken] -> Eval ExpObj
apply fs p i es = case lookup i fs of 
  Nothing -> error $ "Could not lookup func ["++i++"]"
  Just (validators,func) -> validateArgsLength p i (length validators) (length es) >> zipWithM ($) validators es >>= func

validateArgsLength :: Pos -> String -> Int -> Int -> Eval ()
validateArgsLength p i lv le = when (lv /= le) $ Left $ InvalidNbOfArgs p i lv le 

typeMismatch :: Marshallable a => Type -> TypeValidator a
typeMismatch expected e = Left $ TypeMismatch (getP e) expected (getT e)

toTuple :: [FuncEntry ExpToken] -> PairToken -> Eval (String,ExpObj)
toTuple fs (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (lit fs y)
