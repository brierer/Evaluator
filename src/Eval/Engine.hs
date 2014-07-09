module Eval.Engine
( funcs
, showF
, multiF
, meanF
, descF
, tableF
, nTimesF
, takeTF
, takeAF
, sortAF
, sortTF
, colF
, plotLineF
) where

import Prelude hiding             (sum,exp,null)

import qualified Prelude     as P (sum)
import qualified Data.Vector as V (length)

import Control.Monad              (liftM,when)
import Data.Eval                  (EvalError(..),ExpObj(..),EvalFunc,FuncEntry,Func(..))
import Data.List                  (sort,transpose)
import Data.Token                 (Pos)
import Data.Vector                (Vector,fromList,toList)
import Eval.Function              (table,plot,array,str,num,arrayOf,objOf,nonEmpty,(<|>),evalError)
import Statistics.Sample          (mean,variance,skewness,kurtosis)

funcs :: [FuncEntry]
funcs = -- 1 arg functions
        [ ("show",       ([arrayOf $ table <|> plot                               ], Func showL))
        , ("multi",      ([nonEmpty $ arrayOf num                                 ], Func multiL))
        , ("mean",       ([nonEmpty $ arrayOf num                                 ], Func meanL))
        , ("descriptive",([nonEmpty $ arrayOf num                                 ], Func descL))
          -- 2 arg functions
        , ("table",      ([tableArg                , objOf $ arrayOf str          ], Func tableL))
        , ("nTimes",     ([num                     , num                          ], Func nTimesL))
        , ("take",       ([num                     , table <|> array              ], Func takeL))
        , ("sort",       ([num                     , table <|> tableArg           ], Func sortL))
        , ("col",        ([num                     , table <|> tableArg           ], Func colL))
          -- 3 arg functions
        , ("plotLine",   ([arrayOf num             , arrayOf num       , objOf str], Func plotLineL))
        ] where tableArg = nonEmpty $ arrayOf $ nonEmpty array

{-| Function stubs: extract from list and call the actual function -}
showL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
multiL     :: Pos -> [ExpObj] -> EvalFunc ExpObj
meanL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
descL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
tableL     :: Pos -> [ExpObj] -> EvalFunc ExpObj
nTimesL    :: Pos -> [ExpObj] -> EvalFunc ExpObj
takeL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
sortL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
colL       :: Pos -> [ExpObj] -> EvalFunc ExpObj
plotLineL  :: Pos -> [ExpObj] -> EvalFunc ExpObj

showL   p [x]                        = showF   p x;                 showL   _ xs = error $ "Engine::showL  [Unexpected pattern ["++show xs++"]]"
multiL  p [ArrayO _ ns]              = multiF  p ns;                multiL  _ xs = error $ "Engine::multiL [Unexpected pattern ["++show xs++"]]"
meanL   p [ArrayO _ ns]              = meanF   p ns;                meanL   _ xs = error $ "Engine::meanL  [Unexpected pattern ["++show xs++"]]"
descL   p [ArrayO _ ns]              = descF   p ns;                descL   _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
tableL  p [ArrayO _ es, ObjO _ ps]   = tableF  p es ps;             tableL  _ xs = error $ "Engine::tableL [Unexpected pattern ["++show xs++"]]"
nTimesL p [v, NumO _ n]              = nTimesF p v n;               nTimesL _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
takeL   p [NumO _ v,TableO _ ess h]  = takeTF  p (floor v) ess h
takeL   p [NumO _ v,ArrayO _ es]     = takeAF  p (floor v) es;      takeL   _ xs = error $ "Engine::takeL  [Unexpected pattern ["++show xs++"]]"
sortL   p [NumO pn v,TableO _ ess h] = sortTF  p pn (floor v) ess h
sortL   p [NumO pn v,ArrayO _ es]    = sortAF  p pn (floor v) es;   sortL   _ xs = error $ "Engine::sortL  [Unexpected pattern ["++show xs++"]]"

colL         = error "Eval.Function::colL       [Not Implemented]"
plotLineL    = error "Eval.Function::plotLineL  [Not Implemented]"

{-| Actual Functions -}
-- Wrap the topmost result (table or plot) in an object (arbitrary, otherwise the object would be left unchanged)
showF :: Pos ->  ExpObj  -> EvalFunc ExpObj
showF p x = return $ ObjO p [("result",x)]

--Multiply all the given numbers
multiF :: Pos -> [ExpObj] -> EvalFunc ExpObj
multiF p ns = return $ NumO p $ product $ getNums ns

getNums :: [ExpObj] -> [Double]
getNums = map (\(NumO _ x)->x)

--Take the mean of he given numbers
meanF :: Pos -> [ExpObj] -> EvalFunc ExpObj
meanF p ns = return $ NumO p $ mean $ toStatList ns

toStatList :: [ExpObj] -> Vector Double
toStatList = fromList.getNums

--Builds a table of some frequenc descriptive measures for the given numbers
descF :: Pos -> [ExpObj] -> EvalFunc ExpObj
descF  p ns = tableL p [mkColumns p ns,ObjO p []]

mkColumns :: Pos -> [ExpObj] -> ExpObj
mkColumns p ns = ArrayO p [ArrayO p $ map (StrO p.fst) desc, ArrayO p $ map (NumO p.snd) desc ]
  where desc = zip ["count","sum","mean","variance","skewness","kurtosis"] $ map ($ toStatList ns)
                   [ count , sum , mean , variance , skewness , kurtosis]

count :: Vector a -> Double
count = fromIntegral . V.length

sum :: Vector Double -> Double
sum = P.sum.toList

-- Builds a table with the given arrays (columns) and pairs (options dict)
tableF :: Pos-> [ExpObj] -> [(String, ExpObj)] -> EvalFunc ExpObj
tableF p es ps = do ess <- getMatrix es; liftM (TableO p ess) $ getHeader (length ess) ps

getMatrix :: [ExpObj] -> EvalFunc [[ExpObj]]
getMatrix es@(ArrayO _ xs:_) = mapM (getColumn $ length xs) es
getMatrix xs = error $ "Engine::getMatrix [Unexpected pattern ["++show xs++"]]"

getColumn :: Int -> ExpObj -> EvalFunc [ExpObj]
getColumn l (ArrayO p es) = validateLength p l es TableColumnLengthMismatch
getColumn _ x = error $ "Engine::getColumn [Unexpected pattern ["++show x++"]]"

getHeader :: Int -> [(String,ExpObj)] -> EvalFunc [ExpObj]
getHeader l = processHeader l .lookup "col"

processHeader :: Int -> Maybe ExpObj -> EvalFunc [ExpObj]
processHeader _ Nothing              = return []
processHeader l (Just (ArrayO p ss)) = validateLength p l ss TableHeaderLengthMismatch
processHeader _ x = error $ "Engine::processHeader [UnexpectedPattern ["++show x++"]]"

validateLength :: Pos -> Int -> [a] -> (Pos -> Int -> Int -> EvalError) -> EvalFunc [a]
validateLength p expected val errorType = let actual = length val in if expected == actual then return val else evalError $ errorType p expected actual

-- Repeats the given number n times
nTimesF :: Pos -> ExpObj -> Double -> EvalFunc ExpObj
nTimesF p v n = return $ ArrayO p $ replicate (floor n) v

-- Takes from a table the n first rows
takeTF :: Pos -> Int -> [[ExpObj]] -> [ExpObj] -> EvalFunc ExpObj
takeTF  p n ess h = return $ TableO p (map (take n) ess) h

-- Takes from an array the n first elements
takeAF :: Pos -> Int -> [ExpObj] -> EvalFunc ExpObj
takeAF  p n es = return $ ArrayO p $ take n es

-- Sorts a table by its n'th column
sortTF :: Pos -> Pos -> Int -> [[ExpObj]] -> [ExpObj] -> EvalFunc ExpObj
sortTF p pn n ess h = liftM (flip (TableO p) h) $ sortMatrix pn n ess

sortMatrix :: Ord a => Pos -> Int -> [[a]] -> EvalFunc [[a]] 
sortMatrix pn n ess = validateIndex pn n 0 (length ess) >> return (transpose $ map snd $ sort $ zip (ess !! n) $ transpose ess)

validateIndex :: Pos -> Int -> Int -> Int -> EvalFunc ()
validateIndex pn n minL maxL = when (n < minL || n > maxL) $ evalError $ IndexOutOfBounds pn n minL maxL

-- Sorts an array of arrays (matrix) by the n'th array-element (column)
sortAF :: Pos -> Pos -> Int -> [ExpObj] -> EvalFunc ExpObj
sortAF p pn n arrays =  let (fs,ess) = unzip $ map (\(ArrayO q es) -> (ArrayO q,es)) arrays
                     in  liftM (ArrayO p . zipWith ($) fs) (sortMatrix pn n ess)

colF         = error "Eval.Function::colF       [Not Implemented]"
plotLineF    = error "Eval.Function::plotLineF  [Not Implemented]"




        