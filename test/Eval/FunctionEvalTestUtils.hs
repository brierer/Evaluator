{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Eval.FunctionEvalTestUtils where

import Prelude hiding                   (any,null)
import qualified Prelude as P           (null)
import qualified Data.Set as S          (Set,singleton,findMin,findMax,member,insert)

import Control.Arrow                    (second)
import Control.Applicative              (Applicative)
import Control.Monad                    (liftM,liftM2,liftM3,join)
import Control.Monad.State              (State,evalStateT,evalState,put,get)
import Data.Eval                        (EvalError(..),ExpObj(..),Type(..),Eval,Func(..),FuncEntry,EvalFunc,TypeValidator(..))
import Data.List                        (permutations,(\\))
import Data.Maybe                       (isJust)
import Data.Token                       (PairToken(..),IdToken(..),ExpToken(..),Pos)
import Eval.Function                    (Marshallable(..),arrayOf,array,obj,str,num,bool,null,any,lit,(<|>),(<!>),withFuncs)
import Eval.MultiPass                   (mapPair,mapMPair)
import Parser.MonolithicParserTestUtils (Unto(..),Tall(..),IdTA(..),ExpTA(..),ArrayTA(..),ObjTA(..),StrTA(..),NumTA(..),BoolTA(..),NullTA(..),P(..),
                                         sShrink,tShrink,tShrinks,sizes,sized1,liftMF2,liftMF3,liftMF4,uns)
import Test.Framework                   (Arbitrary(..),Gen,elements,choose)

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

data TestToks = TestToks [ExpToken] deriving (Show)
instance Arbitrary TestToks where
  arbitrary                         = mTestToks (tall zero) (tall zero)  arbitrary   arbitrary    arbitrary   arbitrary
  shrink (TestToks [a,o,s,nb,b,nu]) = mTestToks (tShrink a) (tShrink o) (tShrink s) (tShrink nb) (tShrink b) (tShrink nu)
  shrink x                          = error $ "FunctionEvalTestUtils::shrink<TestToks> [Pattern mismatch ["++show x++"]]"
mTestToks a o s n b n' = liftM TestToks $ sequence [liftM un a, liftM un o, liftM un s, liftM un n, liftM un b, liftM un n']
zero = 0 :: Int

data TestObjs = TestObjs [ExpObj] deriving (Show)
instance Arbitrary TestObjs where
  arbitrary               = mTestObjs (tall zero) (tall zero)
  shrink (TestObjs [t,p]) = mTestObjs (tShrink t) (tShrink p)
  shrink x                = error $ "FunctionEvalTestUtils::shrink<TestObjs> [Pattern mismatch ["++show x++"]]"
mTestObjs t p = liftM TestObjs $ sequence [liftM un t, liftM un p]

data ExpOA = ExpOA ExpObj deriving (Eq,Show)
instance Unto ExpOA ExpObj where to = ExpOA; un (ExpOA e) = e
instance Arbitrary ExpOA where arbitrary = sized1 tall; shrink (ExpOA e) = mExpOA (shrinkO e)
instance Tall      ExpOA where                                    tall n =  mExpOA (randomO n)
randomO n = join $ elements $ [mStrO, mNumO, mBoolO, mNullO] ++ if n > 0 then [mTableO n,mPlotO n, mArrayO n, mObjO n] else []
mExpOA = liftM ExpOA

mTableO n = liftM un (tall (n-1) :: Gen TableOA)
mPlotO  n = liftM un (tall (n-1) :: Gen PlotOA)
mArrayO n = liftM un (tall (n-1) :: Gen ArrayOA)
mObjO   n = liftM un (tall (n-1) :: Gen ObjOA)
mStrO     = liftM un (arbitrary  :: Gen StrOA)
mNumO     = liftM un (arbitrary  :: Gen NumOA)
mBoolO    = liftM un (arbitrary  :: Gen BoolOA)
mNullO    = liftM un (arbitrary  :: Gen NullOA)

shrinkO x@(TableO{})  = liftM un $ sShrink (to x :: TableOA)
shrinkO x@(PlotO{})   = liftM un $ sShrink (to x :: PlotOA)
shrinkO x@(ArrayO{})  = liftM un $ sShrink (to x :: ArrayOA)
shrinkO x@(ObjO{})    = liftM un $ sShrink (to x :: ObjOA)
shrinkO x@(StrO{})    = liftM un $ sShrink (to x :: StrOA)
shrinkO x@(NumO{})    = liftM un $ sShrink (to x :: NumOA)
shrinkO x@(BoolO{})   = liftM un $ sShrink (to x :: BoolOA)
shrinkO x@(NullO{})   = liftM un $ sShrink (to x :: NullOA)

data TableOA = TableOA ExpObj deriving (Show)
instance Unto  TableOA ExpObj where to = TableOA; un (TableOA t) = t
instance Arbitrary TableOA where arbitrary = sized1 tall; shrink (TableOA (TableO p a o)) = mTableOA (tShrink p) (tShrink a) (tShrink o)
instance Tall TableOA      where                                                   tall n =  mTableOA  arbitrary  (tall n)    (tall n)
mTableOA = liftMF3 mkTable un un un where mkTable x y = TableOA .TableO x y

data PlotOA = PlotOA ExpObj deriving (Show)
instance Unto PlotOA ExpObj where to = PlotOA; un (PlotOA p) = p
instance Arbitrary PlotOA where arbitrary = sized1 tall; shrink (PlotOA (PlotO p xs ys ps)) = mPlotOA (tShrink p) (tShrink xs)   (tShrink ys)   (tShrink ps)
instance Tall      PlotOA where                                                      tall n =  mPlotOA  arbitrary  (tall n)       (tall n)       (tall n)
mPlotOA = liftMF4 mkPlot un un un un where mkPlot x y z = PlotOA .PlotO x y z

data ArrayOA = ArrayOA ExpObj deriving (Show)
instance Unto  ArrayOA ExpObj where to = ArrayOA; un (ArrayOA a) = a
instance Arbitrary ArrayOA where arbitrary = sized1 tall; shrink (ArrayOA (ArrayO p es)) = mArrayOA (tShrink p) (tShrinks es)
instance Tall      ArrayOA where                                                  tall n =  mArrayOA  arbitrary  (sizes n)
mArrayOA = liftMF2 mkArray un uns where mkArray x = ArrayOA .ArrayO x

data ObjOA =  ObjOA ExpObj deriving (Show)
instance Unto ObjOA ExpObj where to = ObjOA; un (ObjOA o) = o
instance Arbitrary ObjOA where arbitrary = sized1 tall; shrink (ObjOA (ObjO p ps)) = mObjOA (tShrink p) (tShrinks ps)
instance Tall      ObjOA where                                              tall n =  mObjOA  arbitrary  (sizes n)
mObjOA = liftMF2 mkObj un (map (second un)) where mkObj x = ObjOA .ObjO x
instance Unto (String,ExpOA) (String,ExpObj) where un = second un; to = second to
instance Tall (String,ExpOA) where tall n =  liftM2 (,) arbitrary (tall n)

data StrOA =  StrOA ExpObj deriving (Show)
instance Unto StrOA ExpObj where to = StrOA; un (StrOA s) = s
instance Arbitrary StrOA where
  arbitrary                 = mStrOA  arbitrary   arbitrary
  shrink (StrOA (StrO p s)) = mStrOA (tShrink p) (sShrink s)
mStrOA = liftMF2 mkStr un id where mkStr x = StrOA .StrO x

data NumOA =  NumOA ExpObj deriving (Show)
instance Unto NumOA ExpObj where to  = NumOA; un (NumOA n) = n
instance Arbitrary NumOA where
  arbitrary                 = mNumOA arbitrary         arbitrary
  shrink (NumOA (NumO p v)) = mNumOA (tShrink p) (sShrink v)
mNumOA = liftMF2 mkNum un id where mkNum x = NumOA .NumO x

data BoolOA = BoolOA ExpObj deriving (Show)
instance Unto BoolOA ExpObj where to = BoolOA; un (BoolOA b) = b
instance Arbitrary BoolOA where
  arbitrary                   = mBoolOA arbitrary         arbitrary
  shrink (BoolOA (BoolO p v)) = mBoolOA (tShrink p) (sShrink v)
mBoolOA = liftMF2 mkBool un id where mkBool x = BoolOA .BoolO x

data NullOA = NullOA ExpObj deriving (Show)
instance Unto NullOA ExpObj where to = NullOA; un (NullOA n) = n
instance Arbitrary NullOA where
  arbitrary                 = mNullOA arbitrary
  shrink (NullOA (NullO p)) = mNullOA (tShrink p)
mNullOA = liftM (NullOA .NullO .un)

data ExpTS =  ExpTS ExpToken deriving (Eq,Show)
instance Unto ExpTS ExpToken where to = ExpTS; un (ExpTS e) = e
instance Arbitrary ExpTS where arbitrary = sized1 tall; shrink (ExpTS e) = mExpTS (tShrink e)
instance Tall ExpTS where                                         tall n = mExpTS (tall n)
mExpTS = liftM (ExpTS .simplify.un)

data ArrayTS = ArrayTS ExpToken deriving (Show)
instance Unto ArrayTS ExpToken where to = ArrayTS; un (ArrayTS a) = a
instance Arbitrary ArrayTS where arbitrary = sized1 tall; shrink (ArrayTS e) = mArrayTS (tShrink e)
instance Tall      ArrayTS where                                      tall n =  mArrayTS (tall n)
mArrayTS = liftM (ArrayTS .simplify.un)

data ObjTS = ObjTS ExpToken deriving (Show)
instance Unto ObjTS ExpToken where to = ObjTS; un (ObjTS o) = o
instance Arbitrary ObjTS where arbitrary = sized1 tall; shrink (ObjTS e) = mObjTS (tShrink e)
instance Tall      ObjTS where                                    tall n =  mObjTS (tall n)
mObjTS = liftM (ObjTS .simplify.un)

simplify (FuncT{})          = NullT p0 w2
simplify (ArrayT p w es)    = ArrayT p w $ map  simplify          es
simplify (ObjT p w ps)      = ObjT   p w $ map (mapPair simplify) ps
simplify (VarT (IdT p w _)) = NullT  p w
simplify e                  = e

data ArrayTF =  ArrayTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ArrayTF where arbitrary = sized1 tall; shrink (ArrayTF e o i) = mArrayTF (tShrink e) (tShrink o) (tShrink i) 
instance Tall      ArrayTF where                                          tall n = mArrayTF (tall n)    (tall n)     arbitrary
mArrayTF = makeTF ArrayTF 

data ObjTF =  ObjTF ExpToken ExpObj IdToken deriving (Show)
instance Arbitrary ObjTF where arbitrary = sized1 tall; shrink (ObjTF e o i) = mObjTF (tShrink e) (tShrink o) (tShrink i) 
instance Tall      ObjTF where                                        tall n = mObjTF (tall n)    (tall n)     arbitrary
mObjTF = makeTF ObjTF

makeTF f ea oa ia = do e <- ea; o <- oa; i <- ia; return $ f (simplifyF (un i) $ un e) (un o) (un i)

simplifyF i (FuncT w _ _)      = FuncT    w i [] 
simplifyF i (ArrayT p w es)    = ArrayT p w $ map  (simplifyF i)          es
simplifyF i (ObjT p w ps)      = ObjT   p w $ map (mapPair $ simplifyF i) ps
simplifyF _ (VarT (IdT p w _)) = NullT  p w
simplifyF _ e                  = e

data TokOrObj = MkTok ExpToken | MkObj ExpObj deriving (Eq,Show)
instance Marshallable TokOrObj where
  marshall (MkTok t) = marshall t; marshall (MkObj o) = marshall o
  getPos   (MkTok t) = getPos t;     getPos (MkObj o) = getPos o
  getType  (MkTok t) = getType t;   getType (MkObj o) = getType o

data TestIndexesT = TestIndexesT [Int] [Int] deriving (Show)
data TestIndexesO = TestIndexesO [Int] [Int] deriving (Show)
instance Arbitrary TestIndexesT where arbitrary = arbIndexes (permutations [0..5]) TestIndexesT; shrink (TestIndexesT is rest) = shrinkIndexes is rest TestIndexesT
instance Arbitrary TestIndexesO where arbitrary = arbIndexes (permutations [0..7]) TestIndexesO; shrink (TestIndexesO is rest) = shrinkIndexes is rest TestIndexesO
  
arbIndexes is f = do
    i <- choose (0,length is-1)
    j <- choose (2,4)
    let (indexes,rest) = splitAt j (is !! i)
    return $ f indexes rest
    
shrinkIndexes is rest f | length is <= 2 = [] | otherwise = [f (is \\ [i]) (i:rest) | i <- is]

orCase :: Marshallable a => [a] -> [TypeValidator] -> [Int] -> [Int] -> (a -> Eval ExpObj) -> Bool
orCase es vs indexes rest f = 
  let ps = zip es vs
      toOr   = map (ps!!) indexes
      wrongs = map (fst.(ps!!)) rest
      validator    = foldl1 (<|>) (map snd toOr) <!> expectedType
      expectedType = foldl1  Or   (map (getType.fst) toOr)  
  in  forAll toOr   (\(e,v) -> f e == withFuncs [] v e) &&
      forAll wrongs (\e -> Left (TypeMismatch (getPos e) expectedType (getType e)) == withFuncs [] validator e)
      
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

allUniquePos :: [ExpToken] -> [ExpToken]
allUniquePos = flip evalState (S.singleton (0,0)).mapM moo where
  moo (FuncT w i es)  = liftM (FuncT w i) $ mapM moo es
  moo (ArrayT p w es) = do p' <- add p; liftM (ArrayT p' w) $ mapM moo es
  moo (ObjT   p w ps) = do p' <- add p; liftM (ObjT p' w)   $ mapM (mapMPair moo) ps
  moo (StrT   p w s)  = do p' <- add p; return $ StrT p' w s
  moo (NumT p w s n)  = do p' <- add p; return $ NumT p' w s n
  moo (BoolT  p w b)  = do p' <- add p; return $ BoolT p' w b
  moo (NullT  p w)    = do p' <- add p; return $ NullT p' w

allUniquePosO :: [ExpObj] -> [ExpObj]
allUniquePosO = flip evalState (S.singleton (0,0)).mapM moo where
  moo (TableO p a b)  = do p' <- add p; liftM2 (TableO p') (moo a) (moo b)
  moo (PlotO p a b c) = do p' <- add p; liftM3 (PlotO p') (moo a) (moo b) (moo c)
  moo (ArrayO p es)   = do p' <- add p; liftM (ArrayO p') $ mapM moo es
  moo (ObjO   p ps)   = do p' <- add p; liftM (ObjO p')   $ mapM (\(x,y) -> liftM2 (,) (return x) (moo y)) ps
  moo (StrO   p s)    = do p' <- add p; return $ StrO p' s
  moo (NumO p n)      = do p' <- add p; return $ NumO p' n
  moo (BoolO  p b)    = do p' <- add p; return $ BoolO p' b
  moo (NullO  p)      = do p' <- add p; return $ NullO p'

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

leafValidators :: [(String,TypeValidator)]
leafValidators = [("array",array),("obj",obj),("str",str),("num",num),("bool",bool),("null",null)]

findWithPosAndType _ _ []                                                = Nothing
findWithPosAndType p t (e:_)  | getPos e == p && getType e == t               = Just e 
                              | isJust (findWithPosAndType p t $ subs e) = Just e
findWithPosAndType p t (_:es)                                            = findWithPosAndType p t es


class Subs a where subs :: a -> [a]
instance Subs ExpToken where subs (ArrayT _ _ xs) = xs; subs _ = []
instance Subs ExpObj   where subs (ArrayO _   xs) = xs; subs _ = []

applyFunc :: [FuncEntry] -> Pos -> String -> [ExpToken] -> Eval ExpObj
applyFunc fs p n es = withFuncs fs any (mkFunc p n es)

testFunc :: [ExpObj] -> [ExpToken] -> Pos -> String -> ExpObj
testFunc os es p n = fromRight $ applyFunc (mkFuncs os es) p n []

fromRight :: Show a => Eval a -> a
fromRight (Right x) = x
fromRight x         = error $ "FunctionEvalTestUtils::fromRight [Failed pattern match ["++show x++"]]"

mkFunc :: Pos -> String -> [ExpToken] -> ExpToken
mkFunc p n = FuncT w1 (IdT p w2 n)

funcNamesLit   = ["arrayTestF","objTestF","strTestF","numTestF","boolTestF","nullTestF"]
funcNamesNoLit = ["tableTestF","plotTestF"]
mkFuncs :: [ExpObj] -> [ExpToken] -> [FuncEntry]
mkFuncs os es = zipWith f funcNamesNoLit os ++ zipWith g funcNamesLit es
  where f name e = (name,([],Func $ \_ _ -> return e))
        g name e = (name,([],Func $ \_ _ -> testE e))

forAll = flip all
w1 = ""
w2 = ("","")
p0 = (0 :: Int,0 :: Int)

mkEntries ns es os = foldr removeEntry (zip3 (funcNamesNoLit++funcNamesLit) types (map MkObj os++ map MkTok es)) ns
types = [Table,Plot,Arr,Obj,Str,Num,Bool,Null]
removeEntry n = filter $ \(m,_,_) -> n /= m

getTall (FuncT _ _ es)  = length es
getTall (ArrayT _ _ es) = length es
getTall (ObjT _ _ ps)   = length ps

anyCase os es (name,_,MkObj e) = Right e == withFuncs [] any (testFunc os es (getPos e) name)
anyCase os es (name,_,MkTok e) = testS e == withFuncs [] any (testFunc os es (getPos e) name)
                                            
litCase os es (name,_,MkObj e) = Right e == withFuncs [] lit (testFunc os es (getPos e) name)
litCase os es (name,_,MkTok e) = testS e == withFuncs [] lit (testFunc os es (getPos e) name)

toTupleF (PairT _ (IdT _ _ x) y) = liftM2 (,) (return x) (testE y)

testS :: ExpToken -> Eval ExpObj
testS = testF []

testF :: [FuncEntry] -> ExpToken -> Eval ExpObj
testF fs = flip evalStateT fs .testE

testE :: ExpToken -> EvalFunc ExpObj
testE (FuncT _ (IdT p _ i) es) = get >>= \fs -> case lookup i fs of Just (_,Func f) -> mapM testE es >>= f p
testE (ArrayT p _ es) = liftM (ArrayO p) $ mapM testE es
testE (ObjT p _ ps)   = liftM (ObjO p)   $ mapM toTupleF ps
testE (StrT p _ s)    = return $ StrO p s
testE (NumT p _ _ n)  = return $ NumO p n
testE (BoolT p _ b)   = return $ BoolO p b
testE (NullT p _)     = return $ NullO p
testE e               = error $ "FunctionEvalTestUtils::testF [Failed pattern match ["++show e++"]]"

{-| Monomorphism restriction -}
mTestToks :: (Applicative m, Monad m) => m ArrayTS -> m ObjTS -> m StrTA -> m NumTA -> m BoolTA -> m NullTA -> m TestToks
mTestObjs :: (Applicative m, Monad m) => m TableOA -> m PlotOA                                              -> m TestObjs

mExpOA    :: (Applicative m, Monad m) => m ExpObj                                  -> m ExpOA
mTableOA  :: (Applicative m, Monad m) => m P -> m ArrayOA  -> m ObjOA              -> m TableOA
mPlotOA   :: (Applicative m, Monad m) => m P -> m ArrayOA  -> m ArrayOA -> m ObjOA -> m PlotOA
mArrayOA  :: (Applicative m, Monad m) => m P -> m [ExpOA]                          -> m ArrayOA
mObjOA    :: (Applicative m, Monad m) => m P -> m [(String,ExpOA)]                 -> m ObjOA
mStrOA    :: (Applicative m, Monad m) => m P -> m String                           -> m StrOA
mNumOA    :: (Applicative m, Monad m) => m P -> m Double                           -> m NumOA
mBoolOA   :: (Applicative m, Monad m) => m P -> m Bool                             -> m BoolOA
mNullOA   :: (Applicative m, Monad m) => m P                                       -> m NullOA

mExpTS    :: (Applicative m, Monad m) => m ExpTA   -> m ExpTS
mArrayTS  :: (Applicative m, Monad m) => m ArrayTA -> m ArrayTS
mObjTS    :: (Applicative m, Monad m) => m ObjTA   -> m ObjTS

mArrayTF  :: (Applicative m, Monad m) => m ArrayTA -> m ExpOA -> m IdTA -> m ArrayTF
mObjTF    :: (Applicative m, Monad m) => m ObjTA   -> m ExpOA -> m IdTA -> m ObjTF


