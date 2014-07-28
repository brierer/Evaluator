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
instance Arbitrary Show1 where arbitrary = do ((e,o),fs) <- runStateT notArrExp noCallFuncs; return $ Show1 e o fs

data Show2 = Show2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Show2 where arbitrary = do ((a,o),fs) <- runStateT (arrExpOf showableExp notShowableExp) noCallFuncs; return $ Show2 a o fs

data Multi1 = Multi1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Multi1 where arbitrary = do ((e,o),fs) <- runStateT notArrExp noCallFuncs; return $ Multi1 e o fs

data Multi2 = Multi2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Multi2 where arbitrary = do ((a,o),fs) <- runStateT (arrExpOf atomExp notAtomExp) noCallFuncs; return $ Multi2 a o fs

data Mean1 = Mean1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Mean1 where arbitrary = do ((e,o),fs) <- runStateT notArrExp noCallFuncs; return $ Mean1 e o fs

data Mean2 = Mean2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Mean2 where arbitrary = do ((a,o),fs) <- runStateT (arrExpOf atomExp notAtomExp) noCallFuncs; return $ Mean2 a o fs

data Desc1 = Desc1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Desc1 where arbitrary = do ((e,o),fs) <- runStateT notArrExp noCallFuncs; return $ Desc1 e o fs

data Desc2 = Desc2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Desc2 where arbitrary = do ((a,o),fs) <- runStateT (arrExpOf atomExp notAtomExp) noCallFuncs; return $ Desc2 a o fs

data Table1 = Table1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [notArrExp,anyExp]) noCallFuncs; return $ Table1 e1 e2 o1 fs

data Table2 = Table2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table2 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [arrExpOf (arrExpOf' atomExp) notArrExp,anyExp]) noCallFuncs; return $ Table2 e1 e2 o1 fs

data Table3 = Table3 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table3 where
  arbitrary = do
    ((a1,o1),fs0) <- runStateT (arrExpOf atomExp notAtomExp) noCallFuncs
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [arrExpOf (arrExpOf' atomExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1)),anyExp]) fs0
    return $ Table3 e1 e2 o1 fs1

data Table4 = Table4 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table4 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [arrExpOf' $ arrExpOf' atomExp,notObjExp]) noCallFuncs; return $ Table4 e1 e2 o2 fs

data Table5 = Table5 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table5 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [arrExpOf' $ arrExpOf' atomExp,objExpOf (arrExpOf' strExp) notArrExp]) noCallFuncs; return $ Table5 e1 e2 o2 fs

data Table6 = Table6 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table6 where
  arbitrary = do
    ((a1,o1),fs0) <- runStateT (arrExpOf strExp notStrExp) noCallFuncs
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [arrExpOf' $ arrExpOf' atomExp, objExpOf (arrExpOf' strExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1))]) fs0
    return $ Table6 e1 e2 o1 fs1

data NTimes1 = NTimes1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [notNumExp,anyExp]) noCallFuncs; return $ NTimes1 e1 e2 o1 fs

data NTimes2 = NTimes2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp,notNumExp]) noCallFuncs; return $ NTimes2 e1 e2 o2 fs

data Take1 = Take1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Take1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [notNumExp,anyExp]) noCallFuncs; return $ Take1 e1 e2 o1 fs

data Take2 = Take2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Take2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp, chooseExp [plotExp,objExp,strExp,numExp,boolExp,nullExp]]) noCallFuncs; return $ Take2 e1 e2 o2 fs

data Sort1 = Sort1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [notNumExp,anyExp]) noCallFuncs; return $ Sort1 e1 e2 o1 fs

data Sort2 = Sort2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp, chooseExp [plotExp,objExp,strExp,numExp,boolExp,nullExp]]) noCallFuncs; return $ Sort2 e1 e2 o2 fs

data Col1 = Col1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runStateT (sequence [notNumExp,anyExp]) noCallFuncs; return $ Col1 e1 e2 o1 fs

data Col2 = Col2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp, chooseExp [plotExp,objExp,strExp,numExp,boolExp,nullExp]]) noCallFuncs; return $ Col2 e1 e2 o2 fs

data Plot1 = Plot1 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot1 where arbitrary = do ([(e1,o1),(e2,_),(e3,_)],fs) <- runStateT (sequence [notArrExp,anyExp,anyExp]) noCallFuncs; return $ Plot1 e1 e2 e3 o1 fs

data Plot2 = Plot2 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot2 where arbitrary = do ([(e1,o1),(e2,_),(e3,_)],fs) <- runStateT (sequence [arrExpOf numExp notNumExp,anyExp,anyExp]) noCallFuncs; return $ Plot2 e1 e2 e3 o1 fs

data Plot3 = Plot3 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot3 where arbitrary = do ([(e1,_),(e2,o2),(e3,_)],fs) <- runStateT (sequence [arrExpOf' numExp,notArrExp,anyExp]) noCallFuncs; return $ Plot3 e1 e2 e3 o2 fs

data Plot4 = Plot4 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot4 where arbitrary = do ([(e1,_),(e2,o2),(e3,_)],fs) <- runStateT (sequence [arrExpOf' numExp,arrExpOf numExp notNumExp,anyExp]) noCallFuncs; return $ Plot4 e1 e2 e3 o2 fs

data Plot5 = Plot5 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot5 where arbitrary = do ([(e1,_),(e2,_),(e3,o3)],fs) <- runStateT (sequence [arrExpOf' numExp,arrExpOf' numExp,notObjExp]) noCallFuncs; return $ Plot5 e1 e2 e3 o3 fs

data Plot6 = Plot6 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot6 where arbitrary = do ([(e1,_),(e2,_),(e3,o3)],fs) <- runStateT (sequence [arrExpOf' numExp,arrExpOf' numExp,objExpOf strExp notStrExp]) noCallFuncs; return $ Plot6 e1 e2 e3 o3 fs

data Sort3 = Sort3 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort3 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp, arrExpOf (arrExpOf' atomExp) notArrExp]) noCallFuncs; return $ Sort3 e1 e2 o2 fs

data Sort4 = Sort4 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort4 where 
  arbitrary = do
    ((a1,o1),fs0) <- runStateT (arrExpOf atomExp notAtomExp) noCallFuncs
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [numExp, arrExpOf (arrExpOf' atomExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1))]) fs0
    return $ Sort4 e1 e2 o1 fs1

data Col3 = Col3 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col3 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runStateT (sequence [numExp, arrExpOf (arrExpOf' atomExp) notArrExp]) noCallFuncs; return $ Col3 e1 e2 o2 fs

data Col4 = Col4 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col4 where 
  arbitrary = do
    ((a1,o1),fs0) <- runStateT (arrExpOf atomExp notAtomExp) noCallFuncs
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [numExp, arrExpOf (arrExpOf' atomExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1))]) fs0
    return $ Col4 e1 e2 o1 fs1

tableExp = funcValue.un =<< lift (arbitrary :: Gen TableOA)
plotExp  = funcValue.un =<< lift (arbitrary :: Gen PlotOA)
arrExp   = expOrFunc $ liftM un  (arbitrary :: Gen ArrTS)
objExp   = expOrFunc $ liftM un  (arbitrary :: Gen ObjTS)
strExp   = expOrFunc $ liftM un  (arbitrary :: Gen StrTA)
numExp   = expOrFunc $ liftM un  (arbitrary :: Gen NumTA)
boolExp  = expOrFunc $ liftM un  (arbitrary :: Gen BoolTA)
nullExp  = expOrFunc $ liftM un  (arbitrary :: Gen NullTA)

arrExpOf good bad = do
  (ts,os,badT,badO) <- mkElems good bad
  (a,_) <- expOrFunc' (mkArr ts) (arrO os)
  (_,o) <- expOrFunc' badT badO
  return (a,o)

arrExpOf' good = do
  (beforeT,beforeO) <- liftM unzip $ many good
  expOrFunc' (mkArr beforeT) (arrO beforeO)

objExpOf good bad = do
  (ts,os,badT,badO) <- mkElems good bad
  keys              <- replicateM (length ts) $ lift arbitrary
  (a,_) <- expOrFunc' (mkObj $ zipWith mkPair keys ts) (objO $ zip keys os)
  (_,o) <- expOrFunc' badT badO
  return (a,o)

mkElems good bad = do
  (beforeT,beforeO) <- liftM unzip $ many good
  (afterT, afterO)  <- liftM unzip $ many good
  (badT,badO)       <- bad
  return (beforeT ++ [badT] ++ afterT,beforeO ++ [badO] ++ afterO,badT,badO)

many a = (`replicateM` a).(`mod` 5) =<< lift arbitrary

showableExp    = chooseExp [tableExp,plotExp]
atomExp        = chooseExp [                               strExp,numExp,boolExp,nullExp]
anyExp         = chooseExp [tableExp,plotExp,arrExp,objExp,strExp,numExp,boolExp,nullExp]

notShowableExp = chooseExp [                 arrExp,objExp,strExp,numExp,boolExp,nullExp]
notArrExp      = chooseExp [tableExp,plotExp,       objExp,strExp,numExp,boolExp,nullExp]
notObjExp      = chooseExp [tableExp,plotExp,arrExp       ,strExp,numExp,boolExp,nullExp]
notAtomExp     = chooseExp [tableExp,plotExp,arrExp,objExp                              ]
notStrExp      = chooseExp [tableExp,plotExp,arrExp,objExp       ,numExp,boolExp,nullExp]
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


tokPos (FuncT _ (IdT p _ _) _) = p
tokPos (ArrT  p _ _)           = p
tokPos (ObjT  p _ _)           = p
tokPos (StrT  p _ _)           = p
tokPos (NumT  p _ _ _)         = p
tokPos (BoolT p _ _)           = p
tokPos (NullT p _)             = p









