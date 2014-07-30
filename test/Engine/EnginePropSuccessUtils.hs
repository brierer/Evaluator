module Engine.EnginePropSuccessUtils where

import Data.List hiding (any)

import qualified Data.Vector as V

import Control.Monad
import Control.Monad.State
import Data.Eval
import Data.ExpToken
import Data.ExpObj
import Data.Maybe
import Eval.Marshall
import Statistics.Sample
import Test.Framework

import Engine.EnginePropFailureConstraintUtils
import Engine.EnginePropFailureUtils
import Marshall.MarshallUtils
import Parser.ParserUnitUtils
import Parser.ParserUtils

data Show1 = Show1 ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Show1 where arbitrary = do ((e,o'),fs) <- runSuccess $ emptyExp $ arrExpOf' nullExp; p <- randPos; let o = mkObjO p [("result",o')] in return $ Show1 e o p fs

data Show2 = Show2 ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Show2 where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' showableExp; p <- randPos; let o = mkObjO p [("result",o')] in return $ Show2 e o p fs

data Multi = Multi ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Multi where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' atomExp; p <- randPos; let o = mkNumO' p $ getMulti $ elems o' in return $ Multi e o p fs

data Mean = Mean ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Mean where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' atomExp; p <- randPos; let o = mkNumO' p $ getMean $ elems o' in return $ Mean e o p fs

data Desc = Desc ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Desc where arbitrary = do ((e,o'),fs) <- runSuccess $ arrExpOf' atomExp; p <- randPos; let o = mkTableO p (getDesc p $ elems o') [] in return $ Desc e o p fs

data Table1 = Table1 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Table1 where
  arbitrary = do
    (n,_) <- getNM
    ([(e1,o1),(e2,_)],fs) <- runSuccess $ sequence [arrExpOf' $ arrExpOfLength' n atomExp,objExpOfWithout' ["col"] $ arrExpOf' strExp]
    p <- randPos
    let o = mkTableO p (map elems $ elems o1) []
    return $ Table1 e1 e2 o p fs

data Table2 = Table2 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Table2 where
  arbitrary = do
    (n,m) <- getNM
    ([(e1,o1),(e2,o2)],fs) <- runSuccess $ sequence [arrExpOfLength' m (arrExpOfLength' n atomExp),objExpOfWith' ["col"] $ arrExpOfLength' m strExp]
    p <- randPos
    let o = mkTableO p (map elems $ elems o1) (elems $ flip fromMaybe (lookup "col" $ objElems o2) $ error "EnginePropSuccessUtils::arbitrary<Table2> [Couldn't lookup object key [col]]")
    return $ Table2 e1 e2 o p fs

data NTimes = NTimes ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes where arbitrary = do ([(e1,o1),(e2,NumO _ n)],fs) <- runSuccess $ sequence [numExp, numExp]; p <- randPos; let o = mkArrO p $ replicate (floor n) o1 in return $ NTimes e1 e2 o p fs

data Take1 = Take1 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Take1 where
  arbitrary = do
    ([(e1,NumO _ n),(e2,TableO _ ess h)],fs) <- runSuccess $ sequence [numExpIn 1 10,validTableExp]
    p <- randPos
    let o = mkTableO p (map (take $ floor n) ess) h
    return $ Take1 e1 e2 o p fs

data Take2 = Take2 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Take2 where arbitrary = matrixArb Take2 $ \p es n -> mkArrO p $ take n es

data Sort1 = Sort1 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Sort1 where arbitrary = tableArb Sort1 $ \p ess h n -> mkTableO p (sortOn n ess) h

data Sort2 = Sort2 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Sort2 where arbitrary = matrixArb Sort2 $ \p es n -> mkArrO p $ sortArrOn n es

data Col1 = Col1 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Col1 where arbitrary = tableArb Col1 $ \p ess _ n -> mkArrO p $ ess !! n

data Col2 = Col2 ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Col2 where arbitrary = matrixArb Col2 $ \p es n -> let ArrO _ xs = es !! n in mkArrO p xs

data Plott = Plott ExpToken ExpToken ExpToken ExpObj Pos [FuncEntryShow] deriving (Show)
instance Arbitrary Plott where
  arbitrary = do
    ([(e1,ArrO _ ls),(e2,ArrO _ rs)],fs0) <- runSuccess $ sequence [arrExpOf' numExp,arrExpOf' numExp]
    ([h1,h2,h3,h4],fs1) <- runStateT (sequence [objExpOfWithout' ["title","color"] strExp,objExpOfWith' ["title"] strExp,objExpOfWith' ["color"] strExp,objExpOfWith' ["title","color"] strExp]) fs0
    (e3,ObjO _ ps) <- elements [h1,h2,h3,h4]
    p <- randPos
    let o = mkPlotO p (zip ls rs) $ filter ((`elem` ["title","color"]) . fst) ps
    return $ Plott e1 e2 e3 o p fs1

tableArb mk f = do
    ((e2,TableO _ ess h),fs0) <- runSuccess validTableExp
    ((e1,NumO _ n),fs1)       <- runStateT (numExpIn 0 $ length ess - 1) fs0
    p <- randPos
    let o = f p ess h $ floor n
    return $ mk e1 e2 o p fs1

matrixArb mk f = do
    (m1,m2) <- getNM
    ((e2,ArrO _ es),fs0) <- runSuccess $ arrExpOfLength' m1 $ arrExpOfLength' m2 atomExp
    ((e1,NumO _ n),fs1)  <- runStateT (numExpIn 0 $ length es - 1) fs0
    p <- randPos
    let o = f p es $ floor n
    return $ mk e1 e2 o p fs1

successCase r p n args fs = Right r == marshallWith (mkFunc' p n args) (toFuncEntries fs)
successShowCase r p n args fs = let expected = eval r; actual = marshallWith (mkFunc' p n args) $ toFuncEntries fs; showExpected = show expected
                                in expected == actual || (showExpected == show actual && ("NaN" `isInfixOf` showExpected))
elems    (ArrO _ es) = es
objElems (ObjO _ ps) = ps
getMulti = product.getNums
getMean = mean. V.fromList.getNums
getDesc q es = let ns = getNums es; p = Calc q 
               in [map (StrO p)        ["count",            "sum", "mean",           "variance",             "skewness",            "kurtosis"],
                   map (NumO p.($ ns)) [fromIntegral.length ,sum,   mean. V.fromList, variance. V.fromList,   skewness .V.fromList,  kurtosis .V.fromList ]]

getNums = map (\(NumO _ x)->x).filter f where
  f (NumO{}) = True
  f _        = False

eval :: a -> Eval a
eval = return

objExpOfWithout' vs good = do
  (n,_) <- lift getNM
  ks       <- liftM (filter (`notElem` vs)) $ replicateM n $ lift arbitrary
  (ts,os) <- liftM unzip $ replicateM n good
  p <- lift randPos
  expOrFunc' (mkObj' p $ zipWith mkPair ks ts) (mkObjO p $ zip ks os)

objExpOfWith' vs good = do
  (ts,os,_,is) <- mkElems good good $ length vs
  ks <- replicateM (length ts) $ lift arbitrary
  let ps = zip is vs
      keys = zipWith f [0 ..] ks where f j x = fromMaybe x $ lookup j ps
  p <- lift randPos
  expOrFunc' (mkObj' p $ zipWith mkPair keys ts) (mkObjO p $ zip keys os)

sortOn    i ess = transpose $ map snd $ sort $ zip (map ignorePos $ ess !! i) $ transpose ess
sortArrOn i es  = fromPairs fs $ sortOn i ess where (fs,ess) = toPairs es

toPairs   = unzip . map (\(ArrO p es) -> (ArrO p,es))
fromPairs = zipWith ($)

ignorePos (TableO _ ess h) = mkTableO p0 ess h
ignorePos (PlotO  _ ps  h) = mkPlotO  p0 ps  h
ignorePos (ArrO   _ es)    = mkArrO   p0 es
ignorePos (ObjO   _ ps)    = mkObjO   p0 ps
ignorePos (StrO   _ v)     = mkStrO   p0 v
ignorePos (NumO   _ v)     = mkNumO   p0 v
ignorePos (BoolO  _ v)     = mkBoolO  p0 v
ignorePos (NullO  _)       = mkNullO  p0

