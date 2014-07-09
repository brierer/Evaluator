{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTestUtils2 where

import Prelude hiding                   (any,null)

import qualified Data.Set as S          (Set,findMax,findMin,insert,member,singleton)
import qualified Prelude as P           (null)

import Control.Monad                    (liftM,liftM2)
import Control.Monad.State              (State,evalState,get,put)
import Data.Eval                        (Func(..),TypeValidator (..),FuncEntry)
import Data.EvalError                   (EvalError(..))
import Data.ExpObj                      (Type(..),ExpObj(..))
import Data.ExpToken                    (ExpToken (..),IdToken (..),Pos)
import Data.List                        (permutations,(\\))
import Data.Maybe                       (isJust)
import Eval.Function                    (Marshallable (..),any,array,arrayOf,objOf,bool,lit,null,num,obj,str,withFuncs,(<!>),(<|>))
import Eval.MultiPass                   (mapMPair)
import Parser.MonolithicParserTestUtils (IdTA (..),P (..),Tall (..),Unto (..),sized1)
import Test.Framework                   (Arbitrary (..),Gen,NonNegative(..),choose,elements)

import Eval.FunctionEvalTestUtils1

class Is a where
  isFunc     :: a -> Bool
  isVar      :: a -> Bool

  isTable    :: a -> Bool
  isPlot     :: a -> Bool
  isArray    :: a -> Bool
  isObj      :: a -> Bool
  isStr      :: a -> Bool
  isNum      :: a -> Bool
  isBool     :: a -> Bool
  isNull     :: a -> Bool

instance Is ExpToken where
  isTable    = error "FunctionEvalTestUtils::isTable<ExpToken> [Should not be called]"
  isPlot     = error "FunctionEvalTestUtils::isPlot<ExpToken> [Should not be called]"

  isFunc   (FuncT{})  = True; isFunc _  = False
  isVar    (VarT{})   = True; isVar _   = False
  isArray  (ArrayT{}) = True; isArray _ = False
  isObj    (ObjT{})   = True; isObj _   = False
  isStr    (StrT{})   = True; isStr _   = False
  isNum    (NumT{})   = True; isNum _   = False
  isBool   (BoolT{})  = True; isBool _  = False
  isNull   (NullT{})  = True; isNull _  = False

instance Is ExpObj where
  isFunc = error "FunctionEvalTestUtils::isFunc<Obj> [Should not be called]"
  isVar  = error "FunctionEvalTestUtils::isVar<Obj>  [Should not be called]"

  isTable  (TableO{})  = True; isTable _  = False
  isPlot   (PlotO{})   = True; isPlot _   = False
  isArray  (ArrayO{})  = True; isArray _  = False
  isObj    (ObjO{})    = True; isObj _    = False
  isStr    (StrO{})    = True; isStr _    = False
  isNum    (NumO{})    = True; isNum _    = False
  isBool   (BoolO{})   = True; isBool _   = False
  isNull   (NullO{})   = True; isNull _   = False

data InvalidArgsNb = InvalidArgsNb Int Int Pos String ExpToken ExpToken [FuncEntry]
instance Show InvalidArgsNb where show (InvalidArgsNb n m p i g b fs) = unlines ["InvalidArgsNb",show n,show m,show p,show i,show g,show b,show $ map fst fs]
instance Arbitrary InvalidArgsNb where
  arbitrary = do
    NonNegative n <- arbitrary
    NonNegative m <- arbitrary
    let (nbParams,m') = (n `mod` 1000, m `mod` 1000)
    let nbArgs = if nbParams == m' then m' + 1 else m'
    IdT p w name <- liftM un (arbitrary :: Gen IdTA)
    let badFunc  = mkFunc p name $ replicate nbArgs   $ NullT p w
        goodFunc = mkFunc p name $ replicate nbParams $ NullT p w
    return $ InvalidArgsNb nbParams nbArgs p name goodFunc badFunc [(name,(replicate nbParams null,Func $ \_ _ -> return $ NullO p))]

data TokOrObj = MkTok ExpToken | MkObj ExpObj deriving (Eq,Show)
instance Marshallable TokOrObj where
  marshall (MkTok t) = marshall t; marshall (MkObj o) = marshall o
  getPos   (MkTok t) = getPos t;     getPos (MkObj o) = getPos o
  getType  (MkTok t) = getType t;   getType (MkObj o) = getType o

data TestIndexesT = TestIndexesT [ExpToken] [Int] [Int] deriving (Show)
data TestIndexesO = TestIndexesO [ExpObj]   [Int] [Int] deriving (Show)
instance Arbitrary TestIndexesT where
  arbitrary = do
    TestToks es <- arbitrary
    arbIndexes (permutations [0..5]) (TestIndexesT es)
  shrink (TestIndexesT es is rest) = do
    TestToks es' <- shrink $ TestToks es
    shrinkIndexes is rest (TestIndexesT es')

instance Arbitrary TestIndexesO where
  arbitrary = do
    TestObjs os <- arbitrary
    ArrayOA a <- arbitrary
    ObjOA o <- arbitrary
    StrOA s <- arbitrary
    NumOA nb <- arbitrary
    BoolOA b <- arbitrary
    NullOA nu <- arbitrary
    arbIndexes (permutations [0..7]) (TestIndexesO$ os ++ [a,o,s,nb,b,nu])
  shrink (TestIndexesO [t,p,a,o,s,nb,b,nu] is rest) = do
    TestObjs os <- shrink $ TestObjs [t,p]
    ArrayOA a' <- shrink $ ArrayOA a
    ObjOA o' <- shrink $ ObjOA o
    StrOA s' <- shrink $ StrOA s
    NumOA nb' <- shrink $ NumOA nb
    BoolOA b' <- shrink $ BoolOA b
    NullOA nu' <- shrink $ NullOA nu
    shrinkIndexes is rest (TestIndexesO $ os ++ [a',o',s',nb',b',nu'])

arbIndexes is f = do
    i <- choose (0,length is-1)
    j <- choose (2,4)
    let (indexes,rest) = splitAt j (is !! i)
    return $ f indexes rest

shrinkIndexes is rest f | length is <= 2 = [] | otherwise = [f (is \\ [i]) (i:rest) | i <- is]

data ValA a = ValA String TypeValidator
instance Show (ValA a) where show (ValA s _) = "ValA " ++ s
instance Arbitrary (ValA a) where arbitrary = sized1 tall; shrink _  = []
instance Tall      (ValA a) where
  tall 0 = liftM (uncurry ValA) $ elements leafValidators
  tall n = do
    ValA s1 v1 <- tall $ n-1
    ValA s2 v2 <- tall $ n-1
    (s,t) <- elements [(s1,v1),(s2,v2),(s1++" or "++s2,v1<|>v2)]
    liftM (uncurry ValA) $ elements $ ("arrayOf<"++s++">",arrayOf t):leafValidators

data ArgErrorA = ArgErrorA EvalError deriving (Show)
instance Arbitrary ArgErrorA where
  shrink _ = []
  arbitrary = do
    b <- arbitrary
    P pos <- arbitrary
    liftM ArgErrorA $ if b then do
      t1 <- elements types
      t2 <- elements types
      return $ TypeMismatch pos t1 t2
     else return $ IllegalEmpty pos


class AllUniquePos a where
  allUniquePos :: [a] -> [a]
  allUniquePos = flip evalState (S.singleton (0,0)).mapM ensureUnique

  ensureUnique :: a -> State (S.Set Pos) a
instance AllUniquePos ExpToken where
    ensureUnique (FuncT w i es)  = liftM (FuncT w i) $ mapM ensureUnique es
    ensureUnique (ArrayT p w es) = do p' <- add p; liftM (ArrayT p' w) $ mapM ensureUnique es
    ensureUnique (ObjT   p w ps) = do p' <- add p; liftM (ObjT p' w)   $ mapM (mapMPair ensureUnique) ps
    ensureUnique (StrT   p w s)  = do p' <- add p; return $ StrT p' w s
    ensureUnique (NumT p w s n)  = do p' <- add p; return $ NumT p' w s n
    ensureUnique (BoolT  p w b)  = do p' <- add p; return $ BoolT p' w b
    ensureUnique (NullT  p w)    = do p' <- add p; return $ NullT p' w

instance AllUniquePos ExpObj where
  ensureUnique (TableO p a b) = do p' <- add p; liftM2 (TableO p') (mapM (mapM ensureUnique) a) (mapM ensureUnique b)
  ensureUnique (PlotO  p a b) = do p' <- add p; liftM2 (PlotO p')  (mapM (\(x,y) -> liftM2 (,) (ensureUnique x) (ensureUnique y)) a) (mapM ensureUnique b)
  ensureUnique (ArrayO p es)  = do p' <- add p; liftM (ArrayO p')  (mapM ensureUnique es)
  ensureUnique (ObjO   p ps)  = do p' <- add p; liftM (ObjO p')    (mapM (\(x,y) -> liftM2 (,) (return x) (ensureUnique y)) ps)
  ensureUnique (StrO   p s)   = do p' <- add p; return $ StrO p' s
  ensureUnique (NumO p n)     = do p' <- add p; return $ NumO p' n
  ensureUnique (BoolO  p b)   = do p' <- add p; return $ BoolO p' b
  ensureUnique (NullO  p)     = do p' <- add p; return $ NullO p'

instance AllUniquePos b => AllUniquePos (a,b) where ensureUnique (s,e) = liftM2 (,) (return s) $ ensureUnique e

class Elems a where elems :: a -> [a]
instance Elems ExpToken where elems (ArrayT _ _ xs) = xs; elems _ = []
instance Elems ExpObj   where elems (ArrayO _   xs) = xs; elems _ = []

add :: Pos -> State (S.Set Pos) Pos
add p = do
  s <- get
  let p' = if p `S.member` s then notIn s else p
  put $ S.insert p' s
  return p'

notIn s = let (x1,y1) = S.findMin s
              (x2,y2) = S.findMax s
              xs = [x1..x2+1]
              ys = [y1..y2+1]
          in  head $ filter (not.(`S.member`s)) [(x,y) | x <- xs, y <- ys]

leafValidators = [("array",array),("obj",obj),("str",str),("num",num),("bool",bool),("null",null)]

orCase es vs indexes rest f =
  let ps = zip es vs
      toOr   = map (ps!!) indexes
      wrongs = map (fst.(ps!!)) rest
      validator    = foldl1 (<|>) (map snd toOr) <!> expectedType
      expectedType = foldl1  Or   (map (getType.fst) toOr)
  in  forAll toOr   (\(e,v) -> f e == withFuncs [] v e) &&
      forAll wrongs (\e -> Left (TypeMismatch (getPos e) expectedType (getType e)) == withFuncs [] validator e)

caseArrayOf p v xs mkExpectedArray mkArray = f $ allUniquePos xs where
  f es = let arr = mkArray p es in case withFuncs [] (arrayOf v) arr of
    r@(Right _)                        -> r == mkExpectedArray arr
    l@(Left (TypeMismatch pos _ actT)) -> let Just e = findWithPosAndType pos actT es in  l == withFuncs [] v e && f (es \\ [e])

caseObjOf p v xs mkExpectedObj mkObj = f $ allUniquePos xs where
  f ps = let o = mkObj p ps in case withFuncs [] (objOf v) o of
    r@(Right _)                        -> r == mkExpectedObj o
    l@(Left (TypeMismatch pos _ actT)) -> let Just e = findWithPosAndType pos actT $ map snd ps in l == withFuncs [] v e && f (filter (\(_,y) -> y /= e) ps)

findWithPosAndType _ _ []                                                 = Nothing
findWithPosAndType p t (e:_)  | getPos e == p && getType e == t           = Just e
                              | isJust (findWithPosAndType p t $ elems e) = Just e
findWithPosAndType p t (_:es)                                             = findWithPosAndType p t es

mkEntries ns es os = foldr removeEntry (zip3 (funcNamesNoLit++funcNamesLit) types (map MkObj os++ map MkTok es)) ns

anyCase os es (name,_,MkObj e) = Right e == withFuncs [] any (testFunc os es (getPos e) name)
anyCase os es (name,_,MkTok e) = testS e == withFuncs [] any (testFunc os es (getPos e) name)

litCase os es (name,_,MkObj e) = Right e == withFuncs [] lit (testFunc os es (getPos e) name)
litCase os es (name,_,MkTok e) = testS e == withFuncs [] lit (testFunc os es (getPos e) name)


