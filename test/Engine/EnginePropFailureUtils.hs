module Engine.EnginePropFailureUtils where

import Control.Monad.State
import Data.Eval
import Data.ExpToken
import Data.ExpObj
import Eval.Engine
import Test.Framework

import Parser.ParserPropUtils
import Parser.ParserUnitUtils
import Marshall.MarshallPropFailureUtils
import Marshall.MarshallPropUtils
import Marshall.MarshallUtils
import MatchType.MatchTypeUnitUtils
import MatchType.MatchTypePropUtils

data FuncEntryShow = CallFuncEntryShow    String [FiniteType] ExpObj
                   | NoCallFuncEntryShow  String [FiniteType]
                   | FailureFuncEntryShow String [FiniteType] Func
                   | SuccessFuncEntryShow String [FiniteType] Func
instance Show FuncEntryShow where
  show (CallFuncEntryShow s ts o)    = "CallFuncEntryShow "    ++ show s ++ " " ++ show ts ++ " " ++ "("++show o++")"
  show (NoCallFuncEntryShow s ts)    = "NoCallFuncEntryShow "  ++ show s ++ " " ++ show ts
  show (FailureFuncEntryShow s ts _) = "FailureFuncEntryShow " ++ show s ++ " " ++ show ts ++ " (Func "++translate s++")"
  show (SuccessFuncEntryShow s ts _) = "SuccessFuncEntryShow " ++ show s ++ " " ++ show ts ++ " (Func "++translate s++")"

translate s | s `elem` ["show","multi","mean","table","nTimes","take","sort","col","plot"] = s ++ "L"
translate "descriptive" = "descL"
translate x = "<?"++x++"?>"

type GenFunc = StateT [FuncEntryShow] Gen
type GenFuncExp = (ExpToken,ExpObj)

tableExp = funcValue.un =<< lift (arbitrary :: Gen TableOA)
plotExp  = funcValue.un =<< lift (arbitrary :: Gen PlotOA)
arrExp   = expOrFunc $ liftM un  (arbitrary :: Gen ArrTS)
objExp   = expOrFunc $ liftM un  (arbitrary :: Gen ObjTS)
strExp   = expOrFunc $ liftM un  (arbitrary :: Gen StrTA)
numExp   = expOrFunc $ liftM un  (arbitrary :: Gen NumTA)
boolExp  = expOrFunc $ liftM un  (arbitrary :: Gen BoolTA)
nullExp  = expOrFunc $ liftM un  (arbitrary :: Gen NullTA)

arrExpOf good bad = do
  (ts,os,[(badT,badO)],_) <- mkElems good bad 1
  p <- randPos'
  (a,_) <- expOrFunc' (mkArr' p ts) (mkArrO p os)
  (_,o) <- expOrFunc' badT badO
  return (a,o)

arrExpOf' good = do
  (firstT,firstO)   <- good
  (beforeT,beforeO) <- liftM unzip $ many good
  p <- randPos'
  expOrFunc' (mkArr' p $ firstT:beforeT) (mkArrO p $ firstO:beforeO)

objExpOf good bad = do
  (ts,os,[(badT,badO)],_) <- mkElems good bad (1 :: Int)
  keys              <- replicateM (length ts) $ lift arbitrary
  p <- randPos'
  (a,_) <- expOrFunc' (mkObj' p $ zipWith mkPair keys ts) (mkObjO p $ zip keys os)
  (_,o) <- expOrFunc' badT badO
  return (a,o)

objExpOf' good = do
  (firstT,firstO)   <- good
  (beforeT,beforeO) <- liftM unzip $ many good
  keys              <- replicateM (length beforeT + 1) $ lift arbitrary
  p <- randPos'
  expOrFunc' (mkObj' p $ zipWith mkPair keys $ firstT:beforeT) (mkObjO p $ zip keys $ firstO:beforeO)

mkElems good bad n = do
  goods <- many good
  bads   <- replicateM n bad
  ps <- lift $ shuffle $ map Right goods ++ map Left bads
  let is = map fst $ filter (isLeft.snd) $ zip ([0..] :: [Int]) ps
      (ts,os) = unzip $ map (\e -> case e of Right x -> x; Left x -> x) ps
  return (ts,os,bads,is)

isLeft (Left _) = True
isLeft _        = False

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
funcValue o = do s <- newFuncName; modify (CallFuncEntryShow s [] o :); return (mkFunc' (getPos o) s [],o)

newFuncName = liftM getFuncNames get >>= lift.differentOf (\x y -> x ++ "1" ++ y)

differentOf f xs = g [] where g acc = do x <- liftM (f acc) arbitrary; if x `elem` xs then g x else return x

toFuncEntries = map f where
  f (CallFuncEntryShow    s ts o)    = (s,fromFiniteTypes ts,Func $ \_ _ -> return o)
  f (NoCallFuncEntryShow  s ts)      = (s,fromFiniteTypes ts,error $ "EnginePropFailureUtils::toFuncEntries::func [Should not call body of function ["++s++"]]")
  f (FailureFuncEntryShow s ts func) = (s,fromFiniteTypes ts,func)
  f (SuccessFuncEntryShow s ts func) = (s,fromFiniteTypes ts,func)

fromFiniteTypes = map (\(FiniteType t)->t)

getFuncNames = map getFuncName
getFuncName (CallFuncEntryShow    n _ _) = n
getFuncName (NoCallFuncEntryShow  n _)   = n
getFuncName (FailureFuncEntryShow n _ _) = n
getFuncName (SuccessFuncEntryShow n _ _) = n

runNoCall x  = runStateT x $ map (\(s,ts,_) -> NoCallFuncEntryShow s $ map FiniteType ts) funcs
runFailure x = runStateT x $ map f funcs where
  f (s,ts,Func func) = FailureFuncEntryShow s (map FiniteType ts) $ Func $ \p xs -> func p xs >> error ("EnginePropFailureConstraintUtils::toFailureFuncs::func [Function should not succeed ["++s++"]]")

runSuccess x = runStateT x $ map f funcs where f (s,ts,func) = SuccessFuncEntryShow s (map FiniteType ts) func

randPos = liftM un (arbitrary :: Gen P)
randPos' = lift randPos

shuffle = f [] where
  f acc [] = return acc
  f acc xs = elements xs >>= liftM2 f (:acc) (`deleteFirst` xs)

deleteFirst _ [] = []
deleteFirst a (b:bc) | a == b = bc | otherwise = b : deleteFirst a bc

