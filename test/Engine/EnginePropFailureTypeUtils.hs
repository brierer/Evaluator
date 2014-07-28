module Engine.EnginePropFailureTypeUtils where

import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.HasPos
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Eval.Engine
import Parser.ParserPropUtils
import Parser.ParserUnitUtils
import Marshall.MarshallPropFailureUtils
import Marshall.MarshallPropUtils
import Marshall.MarshallUtils
import MatchType.MatchTypeUnitUtils
import MatchType.MatchTypePropUtils

data FuncEntryShow = CallFuncEntryShow   String [FiniteType] ExpObj 
                   | NoCallFuncEntryShow String [FiniteType] deriving (Show)
type GenFunc = StateT [FuncEntryShow] Gen
type GenFuncExp = (ExpToken,ExpObj)

data Show1 = Show1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Show1 where
  arbitrary = do ((e,o),fs) <- runStateT notArrExp noCallFuncs
                 return $ Show1 e o fs
    
data Show2 = Show2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Show2 where
  arbitrary = do ((a,e,o),fs) <- runStateT (arrOf showableExp notShowableExp) noCallFuncs
                 return $ Show2 a e o fs

data NTimes1 = NTimes1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes1 where
  arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [notNumExp,anyExp]) noCallFuncs
                 return $ NTimes1 e1 e2 o1 fs

data NTimes2 = NTimes2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes2 where
  arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp,notNumExp]) noCallFuncs
                 return $ NTimes2 e1 e2 o2 fs

tableExp = funcValue.un =<< lift (arbitrary :: Gen TableOA)
plotExp  = funcValue.un =<< lift (arbitrary :: Gen PlotOA)
arrExp   = expOrFunc $ liftM un  (arbitrary :: Gen ArrTS)
objExp   = expOrFunc $ liftM un  (arbitrary :: Gen ObjTS)
strExp   = expOrFunc $ liftM un  (arbitrary :: Gen StrTA)
numExp   = expOrFunc $ liftM un  (arbitrary :: Gen NumTA)
boolExp  = expOrFunc $ liftM un  (arbitrary :: Gen BoolTA)
nullExp  = expOrFunc $ liftM un  (arbitrary :: Gen NullTA)

arrOf :: GenFunc GenFuncExp -> GenFunc GenFuncExp -> GenFunc (ExpToken,ExpToken,ExpObj)
arrOf good bad = do
  (beforeT,beforeO) <- liftM unzip $ many good
  (afterT, afterO)  <- liftM unzip $ many good
  (badT,badO)       <- bad
  (a,_) <- expOrFunc' (mkArr $ beforeT ++ [badT] ++ afterT) (arrO $ beforeO ++ [badO] ++ afterO)
  (e,o) <- expOrFunc' badT badO
  return (a,e,o)

many a = do
  x <- lift arbitrary
  let n = x `mod` 5
  replicateM n a

showableExp    = chooseExp [tableExp,plotExp]
anyExp         = chooseExp [tableExp,plotExp,arrExp,objExp,strExp,numExp,boolExp,nullExp]

notShowableExp = chooseExp [                 arrExp,objExp,strExp,numExp,boolExp,nullExp]
notArrExp      = chooseExp [tableExp,plotExp,       objExp,strExp,numExp,boolExp,nullExp]
notNumExp      = chooseExp [tableExp,plotExp,arrExp,objExp,strExp,       boolExp,nullExp] 

chooseExp = sequence >=> lift .elements 
expOrFunc ea = do e <- lift ea; let o = unsafeMarshall [] e in expOrFunc' e o
expOrFunc' e o = join $ lift $ elements [return (e,o),funcValue o]
  
funcValue :: ExpObj -> GenFunc GenFuncExp
funcValue o = do s <- newFuncName; modify (CallFuncEntryShow s [] o :); return (mkFunc s [],o)

newFuncName = liftM getFuncNames get >>= lift.differentOf (\x y -> x ++ "1" ++ y) ""
  
differentOf f acc xs = do x <- liftM (f acc) arbitrary; if x `elem` xs then differentOf f x xs else return x

typeFailureCase e t n args fs  = (Left $ TypeMismatch $ TMLeaf (getPos e) t (getRoot e)) == marshallWith (mkFunc n args) (toFuncEntries fs)
toFuncEntries = map f where
  f (CallFuncEntryShow   s ts o) = (s,fromFiniteTypes ts,Func $ \_ _ -> return o)
  f (NoCallFuncEntryShow s ts)   = (s,fromFiniteTypes ts,error $ "EnginePropFailureTypeUtils::noCallFuncs [Should not call body of function ["++s++"]]")
  fromFiniteTypes = map (\(FiniteType t)->t)

noCallFuncs = map (\(s,ts,_) -> NoCallFuncEntryShow s $ map FiniteType ts) funcs
getFuncNames = map f where
  f (CallFuncEntryShow n _ _) = n
  f (NoCallFuncEntryShow n _) = n












