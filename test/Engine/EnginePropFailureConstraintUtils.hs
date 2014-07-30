module Engine.EnginePropFailureConstraintUtils where

import Control.Monad
import Control.Monad.State
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.Maybe
import Eval.Marshall
import Test.Framework

import Engine.EnginePropFailureUtils
import Marshall.MarshallUtils
import Parser.ParserUnitUtils

data MultiEmpty = MultiEmpty ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary MultiEmpty where arbitrary = do ((a,e),fs) <- runFailure $ emptyExp arrExp; return $ MultiEmpty a (getPos e) fs

data MeanEmpty = MeanEmpty ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary MeanEmpty where arbitrary = do ((a,e),fs) <- runFailure $ emptyExp arrExp; return $ MeanEmpty a (getPos e) fs

data DescEmpty = DescEmpty ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary DescEmpty where arbitrary = do ((a,e),fs) <- runFailure $ emptyExp arrExp; return $ DescEmpty a (getPos e) fs

data TableEmpty1 = TableEmpty1 ExpToken ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary TableEmpty1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runFailure $ sequence [emptyExp arrExp,objExpOf' $ arrExpOf' strExp]; return $ TableEmpty1 e1 e2 (getPos o1) fs

data TableEmpty2 = TableEmpty2 ExpToken ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary TableEmpty2 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runFailure $ sequence [arrExpOf (arrExpOf' atomExp) (emptyExp arrExp),objExpOf' $ arrExpOf' strExp]; return $ TableEmpty2 e1 e2 (getPos o1) fs

data SortEmpty1 = SortEmpty1 ExpToken ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary SortEmpty1 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runFailure $ sequence [numExp,emptyExp arrExp]; return $ SortEmpty1 e1 e2 (getPos o2) fs

data SortEmpty2 = SortEmpty2 ExpToken ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary SortEmpty2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runFailure $ sequence [numExp,arrExpOf (arrExpOf' atomExp) (emptyExp arrExp)]; return $ SortEmpty2 e1 e2 (getPos o2) fs

data ColEmpty1 = ColEmpty1 ExpToken ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary ColEmpty1 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runFailure $ sequence [numExp,emptyExp arrExp]; return $ ColEmpty1 e1 e2 (getPos o2) fs

data ColEmpty2 = ColEmpty2 ExpToken ExpToken Pos [FuncEntryShow] deriving (Show)
instance Arbitrary ColEmpty2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runFailure $ sequence [numExp,arrExpOf (arrExpOf' atomExp) (emptyExp arrExp)]; return $ ColEmpty2 e1 e2 (getPos o2) fs

data TableColumnLength = TableColumnLength ExpToken ExpToken Pos Int Int [FuncEntryShow] deriving (Show)
instance Arbitrary TableColumnLength where arbitrary = do; (n,m) <- getNM; ([(e1,o1),(e2,_)],fs) <- runFailure $ sequence [arrExpOfLength (n+m) (arrExpOfLength' n atomExp) (arrExpOfLength' m atomExp),objExpOf' $ arrExpOf' strExp]; return $ TableColumnLength e1 e2 (getPos o1) n m fs

data TableHeaderLength = TableHeaderLength ExpToken ExpToken Pos Int Int [FuncEntryShow] deriving (Show)
instance Arbitrary TableHeaderLength where arbitrary = do (n,m) <- getNM; ([(e1,o1),(e2,_)],fs) <- runFailure $ sequence [arrExpOfLength' n (arrExpOfLength' (n+m) atomExp),objExpOfWith (arrExpOfLength' n strExp) ["col"] (arrExpOfLength' m strExp)]; return $ TableHeaderLength e1 e2 (getPos o1) n m fs

data TableTakeMin = TableTakeMin ExpToken ExpToken Pos Int [FuncEntryShow] deriving (Show)
instance Arbitrary TableTakeMin where arbitrary = do  ([(e1,o1),(e2,_)],fs) <- runFailure $ sequence [numExpIn (-10) 0,validTableExp]; return $ TableTakeMin e1 e2 (getPos o1) (getIntVal o1) fs

data SortIndexOutOfBounds1 = SortIndexOutOfBounds1 ExpToken ExpToken Pos Int Int Int [FuncEntryShow] deriving (Show)
instance Arbitrary SortIndexOutOfBounds1 where arbitrary = indexOutOfBounds1 SortIndexOutOfBounds1

data SortIndexOutOfBounds2 = SortIndexOutOfBounds2 ExpToken ExpToken Pos Int Int Int [FuncEntryShow] deriving (Show)
instance Arbitrary SortIndexOutOfBounds2 where arbitrary = indexOutOfBounds2 SortIndexOutOfBounds2

data ColIndexOutOfBounds1 = ColIndexOutOfBounds1 ExpToken ExpToken Pos Int Int Int [FuncEntryShow] deriving (Show)
instance Arbitrary ColIndexOutOfBounds1 where arbitrary = indexOutOfBounds1 ColIndexOutOfBounds1

data ColIndexOutOfBounds2 = ColIndexOutOfBounds2 ExpToken ExpToken Pos Int Int Int [FuncEntryShow] deriving (Show)
instance Arbitrary ColIndexOutOfBounds2 where arbitrary = indexOutOfBounds2 ColIndexOutOfBounds2

indexOutOfBounds1 f = do ((e2,o2),fs0) <- runFailure validTableExp; let m = tableWidth o2 in do ((e1,o1),fs1) <- runStateT (chooseExp [numExpIn (-10) (-1),numExpIn m 1000]) fs0; return $ f e1 e2 (getPos o1) 0 (m-1) (getIntVal o1) fs1
indexOutOfBounds2  f = do width <- liftM ((+1).(`mod`10)) arbitrary; ((e2,_), fs0) <- runFailure $ arrExpOfLength' width (arrExpOfLength' width atomExp); ((e1,o1),fs1) <- runStateT (chooseExp [numExpIn (-10) (-1),numExpIn width 1000]) fs0; return $ f e1 e2 (getPos o1) 0 (width-1) (getIntVal o1) fs1

emptyExp ea = do (e,o) <- ea; liftM (flip (,) $ clearObjElems o) $ clearTokElems e

clearTokElems (ArrT p w _)               = return $ ArrT p w []
clearTokElems (ObjT p w _)               = return $ ObjT p w []
clearTokElems (StrT p w _)               = return $ StrT p w ""
clearTokElems f@(FuncT _ (IdT _ _ i) _)  = do fs <- get; put $ clearReturnValueElems i fs; return f
clearTokElems x                          = error $ "EnginePropFailureConstraintUtils::clearElems<ExpToken> [Unexpected pattern ["++show x++"]]"

clearObjElems (ArrO p _) = ArrO p []
clearObjElems (ObjO p _) = ObjO p []
clearObjElems (StrO p _) = StrO p ""
clearObjElems x          = error $ "enginePropFailureConstraintUtils::clearObjElems [Unexpected pattern ["++show x++"]]"

clearReturnValueElems i []     = error $ "EnginePropFailureConstraintUtils::clearReturnValueElems [Couldn't lookup function entry ["++i++"]]"
clearReturnValueElems i (x:xs) | getFuncName x == i = g x:xs | otherwise = x:clearReturnValueElems i xs where
  g (CallFuncEntryShow s ts o) = CallFuncEntryShow s ts $ clearObjElems o
  g e                          = e

constraintEmptyCase p = constraintCase $ IllegalEmpty p
constraintCase e n args fs = Left e == marshallWith (mkFunc n args) (toFuncEntries fs)

arrExpOfLength l good bad = do
  i <- lift $ choose (1,l-1)
  (es',os') <- liftM unzip $ replicateM (l+1) good
  (e,o)  <- bad
  let es = map (\(j,x) -> if i == j then e else x) $ zip [0..] es'
      os = map (\(j,x) -> if i == j then o else x) $ zip [0..] os'
  (a,_) <- expOrFunc' (mkArr es) (arrO os)
  return (a,o)

arrExpOfLength' l good = do
  (es,os) <- liftM unzip $ replicateM l good
  expOrFunc' (mkArr es) (arrO os)

objExpOfWith good vs bad = do
  (ts,os,bads,is) <- mkElems good bad $ length vs
  let l = length ts
  ks <- replicateM l $ lift arbitrary
  let ps = zip is vs
      keys = zipWith f [0..] ks where f j x = fromMaybe x $ lookup j ps
  p <- randPos'
  (a,_) <- expOrFunc' (mkObj' p $ zipWith mkPair keys ts) (mkObjO p $ zip keys os)
  (_,o) <- uncurry expOrFunc' $ head bads
  return (a,o)

getNM = do
  [n,m] <- liftM (map ((+1).(`mod` 5))) $ replicateM 2 arbitrary
  return (n,if n == m then m+1 else m)

setNewValue v (NumT p w _ _, NumO q _)              = return (NumT p w (show v) v, NumO q v)
setNewValue v (f@(FuncT _ (IdT _ _ n) _), NumO q _) = modify (map $ \x -> if n == getFuncName x then setNewFuncValue v x else x) >> return (f,NumO q v)

setNewFuncValue v (CallFuncEntryShow s ts (NumO p _)) = CallFuncEntryShow s ts $ NumO p v
setNewFuncValue _ x                                   = error $ "EnginePropFailureConstraintUtils::setNewFuncValue [Unexpected pattern ["++show x++"]]"

numExpIn x y = do i <- lift $ choose (x,y :: Int); numExp >>= setNewValue (fromIntegral i)

tableLength (TableO _ ess _) = length $ head ess
tableLength x                = error $ "EnginePropFailureConstraintUtils::tableLength [Unexpected pattern ["++show x++"]]"

tableWidth (TableO _ ess _) = length ess
tableWidth x                = error $ "EnginePropFailureConstraintUtils::tableWidth [Unexpected pattern ["++show x++"]]"

getIntVal (NumO _ v) | v == fromIntegral (floor v :: Int) = floor v | otherwise = error $ "EnginePropFailureConstraintUtils::getIntVal [Unexpected numeric value ["++show v++"]]"

validTableExp = do
  p <- randPos'
  (n,m) <- lift getNM
  h <- liftM (map snd) $ replicateM n strExp
  funcValue =<< liftM (flip (mkTableO p) h.map (map snd)) (replicateM n $ replicateM m atomExp)

  