module Engine.EnginePropSuccessUtils where

import Data.List hiding (any) 

import qualified Data.Vector as V

import Control.Monad
import Control.Monad.State
import Data.Eval
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.HasPos
import Data.Maybe
import Eval.Engine
import Eval.Marshall
import Statistics.Sample
import Test.Framework

import Engine.EnginePropFailureConstraintUtils
import Engine.EnginePropFailureUtils
import Marshall.MarshallUtils
import MatchType.MatchTypeUnitUtils
import Parser.ParserUnitUtils

data Show1 = Show1 ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Show1 where arbitrary = do ((e,o'),fs) <- runSuccess $ emptyExp $ arrExpOf' nullExp; p <- randPos; let o = ObjO p [("result",o')] in return $ Show1 e o p fs
    
data Show2 = Show2 ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Show2 where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' showableExp; p <- randPos; let o = ObjO p [("result",o')] in return $ Show2 e o p fs

data Multi = Multi ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Multi where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' atomExp; p <- randPos; let o = NumO p $ getMulti $ elems o' in return $ Multi e o p fs

data Mean = Mean ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Mean where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' atomExp; p <- randPos; let o = NumO p $ getMean $ elems o' in return $ Mean e o p fs

data Desc = Desc ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Desc where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' atomExp; p <- randPos; let o = TableO p (getDesc p $ elems o') [] in return $ Desc e o p fs

data Table1 = Table1 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Table1 where 
  arbitrary = do 
    (n,_) <- getNM
    ([(e1,o1),(e2,_)],fs) <- runSuccess $ sequence [arrExpOf' $ arrExpOfLength' n atomExp,objExpOfWithout' "col" $ arrExpOf' strExp]
    p <- randPos
    let o = TableO p (map elems $ elems o1) []
    return $ Table1 e1 e2 o p fs

data Table2 = Table2 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Table2 where 
  arbitrary = do 
    (n,m) <- getNM
    ([(e1,o1),(e2,o2)],fs) <- runSuccess $ sequence [arrExpOfLength' m (arrExpOfLength' n atomExp),objExpOfWith' "col" $ arrExpOfLength' m strExp]
    p <- randPos
    let o = TableO p (map elems $ elems o1) (elems $ flip fromMaybe (lookup "col" $ objElems o2) $ error "EnginePropSuccessUtils::arbitrary<Table2> [Couldn't lookup object key [col]]")
    return $ Table2 e1 e2 o p fs

successCase r p n args fs = Right r == marshallWith (mkFunc' p n args) (toFuncEntries fs)
successShowCase r p n args fs = let expected = eval r; actual = marshallWith (mkFunc' p n args) $ toFuncEntries fs; showExpected = show expected 
                                in expected == actual || (showExpected == show actual && ("NaN" `isInfixOf` showExpected))
elems    (ArrO _ es) = es
objElems (ObjO _ ps) = ps
getMulti = product.getNums
getMean = mean. V.fromList.getNums
getDesc p es = let ns = getNums es in [map (StrO p)        ["count",            "sum", "mean",           "variance",             "skewness",            "kurtosis"],
                                       map (NumO p.($ ns)) [fromIntegral.length ,sum,   mean. V.fromList, variance. V.fromList,   skewness .V.fromList,  kurtosis .V.fromList ]]

getNums = map (\(NumO _ x)->x).filter f where
  f (NumO{}) = True
  f _        = False

eval :: a -> Eval a
eval = return

objExpOfWithout' k good = do
  (n,_) <- lift getNM
  ks'       <- replicateM n $ lift arbitrary
  (ts',os') <- liftM unzip $ replicateM n good
  let (ks,ts,os) = unzip3 $ filter (\(x,_,_) -> x /= k) $ zip3 ks' ts' os'
  p <- lift randPos
  expOrFunc' (mkObj' p $ zipWith mkPair ks ts) (ObjO p $ zip ks os)
  
objExpOfWith' k good = do
  (ts,os,_,_,i) <- mkElems' good good
  ks                  <- replicateM (length ts) $ lift arbitrary
  let keys = zipWith (curry f) [0 ..] ks where f (j,x) | i == j = k | otherwise = x
  p <- lift randPos
  expOrFunc' (mkObj' p $ zipWith mkPair keys ts) (ObjO p $ zip keys os)
  
  
  
  
  
  
  
  
  