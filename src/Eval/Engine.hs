module Eval.Engine
( funcs
, showF
, multiF
, meanF
, descF
, tableF
, nTimesF
, takeTF, takeAF
, sortTF, sortAF
,  colTF,  colAF
, plotF
) where

import Data.List hiding (sum,null) 
import Prelude   hiding (sum,exp,null)

import qualified Data.Vector as V 
import qualified Prelude     as P

import Control.Monad
import Control.Monad.Trans 
import Data.Eval
import Data.Type
import Eval.MatchType
import Data.EvalError          
import Data.ExpObj             
import Data.ExpToken           
import Data.Maybe              
import Statistics.Sample       

funcs :: [FuncEntry]
funcs = -- 1 arg functions
        [ ("show",       [showables                                ], Func showL)
        , ("multi",      [atoms                                    ], Func multiL)
        , ("mean",       [atoms                                    ], Func meanL)
        , ("descriptive",[atoms                                    ], Func descL)
          -- 2 arg functions
        , ("table",      [tableArg , ObjOf $ ArrOf Str             ], Func tableL)
        , ("nTimes",     [Num      , Num                           ], Func nTimesL)
        , ("take",       [Num      , Or [Table, arr]               ], Func takeL)
        , ("sort",       [Num      , Or [Table,tableArg]           ], Func sortL)
        , ("col",        [Num      , Or [Table,tableArg]           ], Func colL)
          -- 3 arg functions
        , ("plot",       [ArrOf Num, ArrOf Num          , ObjOf Str], Func plotL)
        ]
  where showables = ArrOf $ Or [Table, Plot]
        atoms     = ArrOf atom
        tableArg  = ArrOf $ ArrOf atom
        atom      = Or [Str,Num,Bool,Null]

{-| Function stubs: extract from list, check some simple constraints and call the actual function -}
showL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
multiL     :: Pos -> [ExpObj] -> EvalFunc ExpObj
meanL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
descL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
tableL     :: Pos -> [ExpObj] -> EvalFunc ExpObj
nTimesL    :: Pos -> [ExpObj] -> EvalFunc ExpObj
takeL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
sortL      :: Pos -> [ExpObj] -> EvalFunc ExpObj
colL       :: Pos -> [ExpObj] -> EvalFunc ExpObj
plotL      :: Pos -> [ExpObj] -> EvalFunc ExpObj

showL   p [x]                               =                      showF   p x;                   showL   _ xs = error $ "Engine::showL  [Unexpected pattern ["++show xs++"]]"
multiL  p [a@(ArrO _ ns)]                   = nonEmpty a >>        multiF  p ns;                  multiL  _ xs = error $ "Engine::multiL [Unexpected pattern ["++show xs++"]]"
meanL   p [a@(ArrO _ ns)]                   = nonEmpty a >>        meanF   p ns;                  meanL   _ xs = error $ "Engine::meanL  [Unexpected pattern ["++show xs++"]]"
descL   p [a@(ArrO _ ns)]                   = nonEmpty a >>        descF   p ns;                  descL   _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
tableL  p [a@(ArrO _ es), ObjO _ ps]        = nonEmptyMatrix a >>  tableF  p es ps;               tableL  _ xs = error $ "Engine::tableL [Unexpected pattern ["++show xs++"]]"
                                        
nTimesL p [v, NumO _ n]                     =                      nTimesF p v n;                 nTimesL _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
                                                                   
takeL   p [NumO _ v,TableO _ ess h]         =                      takeTF  p (floor v) ess h
takeL   p [NumO _ v,ArrO _ es]              =                      takeAF  p (floor v) es;        takeL   _ xs = error $ "Engine::takeL  [Unexpected pattern ["++show xs++"]]"
                                                                   
sortL   p [NumO pn v,TableO _ ess h]        =                      sortTF  p pn (floor v) ess h
sortL   p [NumO pn v,a@(ArrO _ es)]         = nonEmptyMatrix a >>  sortAF  p pn (floor v) es;     sortL   _ xs = error $ "Engine::sortL  [Unexpected pattern ["++show xs++"]]"
                                                                   
colL    p [NumO pn v,TableO _ ess _]        =                      colTF   p pn (floor v) ess;
colL    p [NumO pn v,a@(ArrO _ es)]         = nonEmptyMatrix a >>  colAF   p pn (floor v) es;     colL    _ xs = error $ "Engine::colL   [Unexpected pattern ["++show xs++"]]"
                                                                   
plotL   p [ArrO _ xs, ArrO _ ys, ObjO _ ps] =                      plotF p xs ys ps;              plotL   _ xs = error $ "Engine::plotL  [Unexpected pattern ["++show xs++"]]"

{-| Simple contraints -}
nonEmpty :: ExpObj -> EvalFunc ()
nonEmpty (ArrO p es) = nonEmpty' p es
nonEmpty x           = error $ "Eval.Engine::nonEmpty [Unexpected pattern ["++show x++"]]" 

nonEmpty' :: Pos -> [a] -> EvalFunc()
nonEmpty' p xs = when (P.null xs) $ lift $ Left $ IllegalEmpty p

nonEmptyMatrix :: ExpObj -> EvalFunc ()
nonEmptyMatrix a@(ArrO _ es) = nonEmpty a >> mapM_ nonEmpty es    
nonEmptyMatrix x             = error $ "Eval.Engine::nonEmptyMatrix [Unexpected pattern ["++show x++"]]"    

{-| Actual Functions -}
-- Wrap the topmost result (table or plot) in an object (arbitrary, otherwise the object would be left unchanged)
showF :: Pos ->  ExpObj  -> EvalFunc ExpObj
showF p x = return $ ObjO p [("result",x)]

--Multiply all the given numbers
multiF :: Pos -> [ExpObj] -> EvalFunc ExpObj
multiF p ns = return $ NumO p $ product $ getNums ns

getNums :: [ExpObj] -> [Double]
getNums = map (\(NumO _ x)->x).filter f where
  f (NumO{}) = True
  f _        = False

--Take the mean of he given numbers
meanF :: Pos -> [ExpObj] -> EvalFunc ExpObj
meanF p ns = return $ NumO p $ mean $ toStatList ns

toStatList :: [ExpObj] -> V.Vector Double
toStatList = V.fromList.getNums

--Builds a table of some frequenc descriptive measures for the given numbers
descF :: Pos -> [ExpObj] -> EvalFunc ExpObj
descF  p ns = tableL p [mkColumns p ns,ObjO p []]

mkColumns :: Pos -> [ExpObj] -> ExpObj
mkColumns p ns = ArrO p [ArrO p $ map (StrO p.fst) desc, ArrO p $ map (NumO p.snd) desc ]
  where desc = zip ["count","sum","mean","variance","skewness","kurtosis"] $ map ($ toStatList ns)
                   [ count , sum , mean , variance , skewness , kurtosis]

count :: V.Vector a -> Double
count = fromIntegral . V.length

sum :: V.Vector Double -> Double
sum = P.sum. V.toList

-- Builds a table with the given arrays (columns) and pairs (options dict)
tableF :: Pos-> [ExpObj] -> [(String, ExpObj)] -> EvalFunc ExpObj
tableF p es ps = do ess <- getMatrix es; liftM (TableO p ess) $ getHeader (length ess) ps

getMatrix :: [ExpObj] -> EvalFunc [[ExpObj]]
getMatrix es@(ArrO _ xs:_) = mapM (getColumn $ length xs) es
getMatrix xs = error $ "Engine::getMatrix [Unexpected pattern ["++show xs++"]]"

getColumn :: Int -> ExpObj -> EvalFunc [ExpObj]
getColumn l (ArrO p es) = validateLength p l es TableColumnLengthMismatch
getColumn _ x = error $ "Engine::getColumn [Unexpected pattern ["++show x++"]]"

getHeader :: Int -> [(String,ExpObj)] -> EvalFunc [ExpObj]
getHeader l = processHeader l .lookup "col"

processHeader :: Int -> Maybe ExpObj -> EvalFunc [ExpObj]
processHeader _ Nothing              = return []
processHeader l (Just (ArrO p ss)) = validateLength p l ss TableHeaderLengthMismatch
processHeader _ x = error $ "Engine::processHeader [UnexpectedPattern ["++show x++"]]"

validateLength :: Pos -> Int -> [a] -> (Pos -> Int -> Int -> EvalError) -> EvalFunc [a]
validateLength p expected val errorType = let actual = length val in if expected == actual then return val else lift $ Left $ errorType p expected actual

-- Repeats the given number n times
nTimesF :: Pos -> ExpObj -> Double -> EvalFunc ExpObj
nTimesF p v n = return $ ArrO p $ replicate (floor n) v

-- Takes from a table the n first rows
takeTF :: Pos -> Int -> [[ExpObj]] -> [ExpObj] -> EvalFunc ExpObj
takeTF  p n ess h = return $ TableO p (map (take n) ess) h

-- Takes from an array the n first elements
takeAF :: Pos -> Int -> [ExpObj] -> EvalFunc ExpObj
takeAF  p n es = return $ ArrO p $ take n es

-- Sorts a table by its n'th column
sortTF :: Pos -> Pos -> Int -> [[ExpObj]] -> [ExpObj] -> EvalFunc ExpObj
sortTF p pn n ess h = liftM (flip (TableO p) h) $ sortMatrix pn n ess

sortMatrix :: Ord a => Pos -> Int -> [[a]] -> EvalFunc [[a]]
sortMatrix pn n ess = validateIndex pn n 0 (length ess - 1) >> return (transpose $ map snd $ sort $ zip (ess !! n) $ transpose ess)

validateIndex :: Pos -> Int -> Int -> Int -> EvalFunc ()
validateIndex pn n minL maxL = when (n < minL || n > maxL) $ lift $ Left $ IndexOutOfBounds pn n minL maxL

-- Sorts an array of arrays (matrix) by the n'th array-element (column)
sortAF :: Pos -> Pos -> Int -> [ExpObj] -> EvalFunc ExpObj
sortAF p pn n arrays =  let (fs,ess) = unzip $ map (\(ArrO q es) -> (ArrO q,es)) arrays
                     in  liftM (ArrO p . zipWith ($) fs) (sortMatrix pn n ess)

-- Extracts from a table the n'th column as an array
colTF :: Pos -> Pos -> Int -> [[ExpObj]] -> EvalFunc ExpObj
colTF p pn n ess = validateIndex pn n 0 (length ess - 1) >> return (ArrO p $ ess !! n)

-- Extracts from an array of arrays (matrix) the n'th array-element (column)
colAF :: Pos -> Pos -> Int -> [ExpObj] -> EvalFunc ExpObj
colAF p pn n arrays = validateIndex pn n 0 (length arrays - 1) >> return (let ArrO _ es = arrays !! n in ArrO p es)

-- Plots a line graph using the (x,y) points
plotF :: Pos -> [ExpObj] -> [ExpObj] -> [(String,ExpObj)] -> EvalFunc ExpObj
plotF p xs ys ps = return $ PlotO p (zip xs ys) $ getAttributes ps

getAttributes :: [(String,ExpObj)] -> [(String,ExpObj)]
getAttributes ps = get ps "title" ++ get ps "color"

get :: [(String, ExpObj)] -> String -> [(String, ExpObj)]
get ps n = maybeToList $ do r <- lookup n ps; return (n,r)


        