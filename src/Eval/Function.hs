module Eval.Function
( Marshallable(..)
, any,   anyType
, noLit, noLitType
, lit,   litType
, (<|>), (<!>)
, applyFunc
, withFuncs
) where

import Prelude hiding      (any,null)

import Control.Monad.State (evalStateT,get,lift)
import Control.Monad       (liftM,liftM2,zipWithM,when)
import Data.Eval           (Type(..),ExpObj(..),EvalError(..),Eval,FuncEntry,EvalFunc,TypeValFunc,TypeValidator(..),Func(..))
import Data.Token          (PairToken(..),IdToken(..),ExpToken(..),Pos)

class Marshallable a where
  arrayOf  :: TypeValidator a -> TypeValidator a

  table   :: TypeValidator a
  plot    :: TypeValidator a
  funCall :: TypeValidator a
  array   :: TypeValidator a; array = arrayOf any
  obj     :: TypeValidator a
  str     :: TypeValidator a
  num     :: TypeValidator a
  bool    :: TypeValidator a
  null    :: TypeValidator a

  getP :: a -> Pos
  getT :: a -> Type

instance Marshallable ExpToken where
  table = typeMismatch litType
  plot  = typeMismatch litType

  funCall   = matchType FunCall $ valFunc applyFunc
  
  arrayOf v = matchType Arr  $ \(ArrayT p _ es) -> liftM (ArrayO p) $ mapM (valFunc v) es
  obj       = matchType Obj  $ \(ObjT p _ ps)   -> liftM (ObjO p)   $ mapM toTuple ps
  str       = matchType Str  $ \(StrT p _ s)    -> return $ StrO p s
  num       = matchType Num  $ \(NumT p _ _ n)  -> return $ NumO p n
  bool      = matchType Bool $ \(BoolT p _ b)   -> return $ BoolO p b
  null      = matchType Null $ \(NullT p _)     -> return $ NullO p

  getP (FuncT _ (IdT p _ _) _) = p
  getP (ArrayT p _ _)          = p
  getP (ObjT p _ _)            = p
  getP (StrT p _ _)            = p
  getP (NumT p _ _ _)          = p
  getP (BoolT p _ _)           = p
  getP (NullT p _)             = p
  getP e                       = error $ "Eval.Function::getP<ExpToken> [Failed pattern match ["++show e++"]]"

  getT (FuncT{})      = FunCall
  getT (ArrayT{})     = Arr
  getT (ObjT{})       = Obj
  getT (StrT{})       = Str
  getT (NumT{})       = Num
  getT (BoolT{})      = Bool
  getT (NullT{})      = Null
  getT e              = error $ "Eval.Function::getT<ExpToken> [Failed pattern match ["++show e++"]]"

instance Marshallable ExpObj where
  funCall = typeMismatch FunCall

  arrayOf v = matchType Arr $ \(ArrayO p es) -> liftM (ArrayO p) $ mapM (valFunc v) es
  table     = matchType Table return
  plot      = matchType Plot  return
  obj       = matchType Obj   return
  str       = matchType Str   return
  num       = matchType Num   return
  bool      = matchType Bool  return
  null      = matchType Null  return

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
  getT (ArrayO{})  = Arr
  getT (ObjO{})    = Obj
  getT (StrO{})    = Str
  getT (NumO{})    = Num
  getT (BoolO{})   = Bool
  getT (NullO{})   = Null

any   :: Marshallable a => TypeValidator a
noLit :: Marshallable a => TypeValidator a
lit   :: Marshallable a => TypeValidator a

any   = table <|> plot <|> funCall <|> array <|> obj <|> str <|> num <|> bool <|> null <!> anyType
noLit = table <|> plot                                                                 <!> noLitType
lit   =                    funCall <|> array <|> obj <|> str <|> num <|> bool <|> null <!> litType

anyType   :: Type
litType   :: Type
noLitType :: Type

anyType   = foldl1 Or [noLitType,litType]
noLitType = foldl1 Or [Table,Plot]
litType   = foldl1 Or [Arr,Obj,Str,Num,Bool,Null]


(<|>) :: TypeValidator a -> TypeValidator a -> TypeValidator a
infixl 3 <|>
TypeVal a <|> TypeVal b = TypeVal $ \x -> do st <- get; case evalStateT (a x) st of Right r -> return r; Left _ -> b x

infixl 3 <!>
(<!>) :: Marshallable a => TypeValidator a -> Type -> TypeValidator a
(<!>) v = (<|>) v . typeMismatch

{-| Function validation and application -}
applyFunc :: TypeValidator ExpToken
applyFunc  = TypeVal $ \(FuncT _ (IdT p _ i) es) -> get >>= \fs -> case lookup i fs of
    Nothing -> error $ "Could not lookup func ["++i++"]"
    Just (validators,Func func) -> validateArgsLength p i (length validators) (length es) >> zipWithM valFunc validators es >>= func

validateArgsLength :: Pos -> String -> Int -> Int -> EvalFunc ExpToken ()
validateArgsLength p i lv le = when (lv /= le) $ lift $ Left $ InvalidNbOfArgs p i lv le

withFuncs :: Marshallable a => [FuncEntry a] -> TypeValidator a -> a -> Eval ExpObj
withFuncs fs v e = evalStateT (valFunc v e) fs

{-| Helper functions -}
matchType :: Marshallable a => Type -> TypeValFunc a -> TypeValidator a
matchType t k = TypeVal $ \e -> if t == getT e  then k e else valFunc (typeMismatch t) e

typeMismatch :: Marshallable a => Type -> TypeValidator a
typeMismatch expected = TypeVal $ \e -> lift $ Left $ TypeMismatch (getP e) expected (getT e)

toTuple :: PairToken -> EvalFunc ExpToken (String,ExpObj)
toTuple (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (valFunc lit y)



