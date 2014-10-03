module Eval.Engine
( funcs
, getAllFunctionsNames  
, showL,   showF
, multiL,  multiF
, meanL,   meanF
, descL,   descF
, tableL,  tableF
, nTimesL, nTimesF
, takeL,   takeTF, takeAF
, sortL,   sortTF, sortAF
, colL,    colTF,  colAF
, plotL,   plotF
) where

import Data.List hiding (sum,null)
import Prelude   hiding (sum,exp,null)

import qualified Data.Vector as V
import qualified Numeric.Matrix as M 
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
        , ("sum",        [atoms                                    ], Func sumL)
        , ("descriptive",[atoms                                    ], Func descL)
        , ("transpose",  [Or [tableArg]                            ], Func transL)
        , ("fst",        [ArrOf Eval.MatchType.any                 ], Func fstL)
         ,("snd",        [ArrOf Eval.MatchType.any                 ], Func sndL)
          -- 2 arg functions
        , ("prdMatrix",  [Table, Table]                            ,  Func prdMatrixL)
        , ("table",      [tableArg , ObjOf tableParam              ], Func tableL)
        , ("sums",       [Table                                    ], Func sumtableL)
        , ("nTimes",     [Num      , Num                           ], Func nTimesL)
        , ("take",       [Num      , Or [Table, arr]               ], Func takeL)
        , ("sort",       [Num      , Or [Table,tableArg]           ], Func sortL)
        , ("col",        [Num      , Or [Table,tableArg]           ], Func colL)
        , ("row",        [Num      , Or [Table]                    ], Func rowL)
        , ("diff",       [atoms    , atoms                    ], Func diffL)
        , ("concat",     [Or [tableArg]                     ], Func concatL)
        , ("break",      [Str, Bool]                                , Func breakL)
          -- 3 arg functions
        , ("plot",       [atoms, ArrOf Num          , ObjOf plotParam], Func plotL)
        ]
  where showables = ArrOf $ Or [Table, Plot, Widget]
        atoms     = ArrOf atom
        tableArg  = ArrOf $ ArrOf atom
        atom      = Or [Str,Num,Bool,Null]
        tableParam = [("col", Or [(ArrOf Str),Bool]),("row", Or [(ArrOf Str),Bool]),("title",Str),("spare",Num),("color", Str)]
        plotParam  = [("color", Str),("title",Str)]

getName :: FuncEntry -> String
getName (x,_,_) = x

getAllFunctionsNames :: [String]
getAllFunctionsNames = fmap getName funcs     

{-| Function stubs: extract from list, check some simple constraints and call the actual function -}
showL      :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
multiL     :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
meanL      :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
descL      :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
transL     :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
tableL     :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
nTimesL    :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
takeL      :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
sortL      :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
colL       :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
plotL      :: ObjPos -> [ExpObj] -> EvalFunc ExpObj

showL   p [x]                               =                      showF   p x;                   showL   _ xs = error $ "Engine::showL  [Unexpected pattern ["++show xs++"]]"
multiL  p [a@(ArrO _ ns)]                   =        multiF  p ns;                  multiL  _ xs = error $ "Engine::multiL [Unexpected pattern ["++show xs++"]]"
meanL   p [a@(ArrO _ ns)]                   =        meanF   p ns;                  meanL   _ xs = error $ "Engine::meanL  [Unexpected pattern ["++show xs++"]]"
sumL    p [a@(ArrO _ ns)]                   =        sumF    p ns;                  sumL   _ xs = error $ "Engine::meanL  [Unexpected pattern ["++show xs++"]]"
fstL    p [a@(ArrO _ ns)]                   =        fstF    p ns;                   fstL   _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
sndL    p [a@(ArrO _ ns)]                   =        sndF    p ns;                   sndL   _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
descL   p [a@(ArrO _ ns)]                   =        descF   p ns;                  descL   _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
transL  p [a@(ArrO _ es)]                   =        transF  p es;                  transL  _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"
tableL  p [a@(ArrO _ es), ObjO _ ps]        =        tableF  p es ps;                tableL  _ xs = error $ "Engine::tableL [Unexpected pattern ["++show xs++"]]"
sumtableL  p [a@(TableO _ es h)]                =        sumtableF  p es;            sumtableL  _ xs = error $ "Engine::tableL [Unexpected pattern ["++show xs++"]]"
prdMatrixL p [a@(TableO _ es h),(TableO _ es1 h1)]=      prdMatrixF p es es1;        prdMatrixL  _ xs = error $ "Engine::tableL [Unexpected pattern ["++show xs++"]]"

nTimesL p [v, NumO _ n]                     =                      nTimesF p v n;                 nTimesL _ xs = error $ "Engine::descL  [Unexpected pattern ["++show xs++"]]"

takeL   p [NumO q v,TableO _ ess  h]        =                      takeTF  p q (floor v) ess h
takeL   p [NumO _ v,ArrO _ es]              =                      takeAF  p   (floor v) es;      takeL   _ xs = error $ "Engine::takeL  [Unexpected pattern ["++show xs++"]]"

sortL   p [NumO pn v,TableO _ ess h]        =                      sortTF  p pn (floor v) ess h
sortL   p [NumO pn v,a@(ArrO _ es)]         =        sortAF  p pn (floor v) es;     sortL   _ xs = error $ "Engine::sortL  [Unexpected pattern ["++show xs++"]]"

colL    p [NumO pn v,TableO _ ess _]        =                      colTF   p pn (floor v) ess;
colL    p [NumO pn v,a@(ArrO _ es)]         =        colAF   p pn (floor v) es;     colL    _ xs = error $ "Engine::colL   [Unexpected pattern ["++show xs++"]]"


rowL    p [NumO pn v,TableO _ ess _]        =                      rowTF   p pn (floor v) ess;

concatL    p [a@(ArrO _ es)]         =                              concatF p es;

plotL   p [ArrO _ xs, ArrO _ ys, ObjO _ ps] =                      plotF p xs ys ps;              plotL   _ xs = error $ "Engine::plotL  [Unexpected pattern ["++show xs++"]]"

diffL  p [a@(ArrO _ es),b@(ArrO _ es1)]  = diffF p es es1 
{-| Simple contraints -}
nonEmpty :: ExpObj -> EvalFunc ()
nonEmpty (ArrO p es) = nonEmpty' p es
nonEmpty x           = error $ "Eval.Engine::nonEmpty [Unexpected pattern ["++show x++"]]"

nonEmpty' :: ObjPos -> [a] -> EvalFunc()
nonEmpty' p xs = when (P.null xs) $ lift $ Left $ IllegalEmpty $ fromObjPos p

nonEmptyMatrix :: ExpObj -> EvalFunc ()
nonEmptyMatrix a@(ArrO _ es) = nonEmpty a >> mapM_ nonEmpty es
nonEmptyMatrix x             = error $ "Eval.Engine::nonEmptyMatrix [Unexpected pattern ["++show x++"]]"

{-| Actual Functions -}
-- Wrap the topmost result (table or plot) in an object (arbitrary, otherwise the object would be left unchanged)
showF :: ObjPos ->  ExpObj  -> EvalFunc ExpObj
showF p x = return $ ObjO p [("result",x)]

--Multiply all the given numbers
multiF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
multiF p ns = return $ NumO p $ product $ getNums ns

getNums :: [ExpObj] -> [Double]
getNums = map (\(NumO _ x)->x).filter f where
  f (NumO{}) = True
  f _        = False

--Take the mean of he given numbers
meanF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
meanF p ns = return $ NumO p $ mean $ toStatList ns

--Take the sum of he given numbers
sumF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
sumF p ns = return $ NumO p $ sum $ toStatList ns

sumF' :: ObjPos -> [ExpObj] -> EvalFunc [ExpObj]
sumF' p ns = return $ [NumO p $ sum $ toStatList ns]

toStatList :: [ExpObj] -> V.Vector Double
toStatList = V.fromList.getNums

--Builds a table of some frequenc descriptive measures for the given numbers
descF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
descF  p ns = tableL p [mkColumns p ns,ObjO p []]

mkColumns :: ObjPos -> [ExpObj] -> ExpObj
mkColumns p ns = ArrO p [ArrO p $ map (StrO p.fst) desc, ArrO p $ map (NumO p.snd) desc ]
  where desc = zip ["count","sum","mean","variance","skewness","kurtosis"] $ map ($ toStatList ns)
                   [ count , sum , mean , variance , skewness , kurtosis]

count :: V.Vector a -> Double
count = fromIntegral . V.length

sum :: V.Vector Double -> Double
sum = P.sum. V.toList

-- Builds a table with the given arrays (columns) and pairs (options dict)
tableF :: ObjPos-> [ExpObj] -> [(String, ExpObj)] -> EvalFunc ExpObj
tableF p es ps = do ess <- getMatrix es; return $  TableO p ess (Param  $ (getTableAttributes ps))

--sumtableF :: ObjPos-> [ExpObj] -> [(String, ExpObj)] -> EvalFunc ExpObj
--sumtableF p es ps = do ess <- getMatrix es; ss <- ((mapM (sumF' p) ess)); return $ ArrO p [(TableO p ess (Param  $ (getTableAttributes ps))),(TableO p ss (Param  $ replaceAttributes (getTableAttributes ps) "col" (ArrO p [])))]
sumtableF :: ObjPos-> [[ExpObj]] -> EvalFunc ExpObj
sumtableF p es = do ss <- ((mapM (sumF' p) es)); return $ ArrO p (map (ArrO p) ss)

prdMatrix :: ObjPos -> [[ExpObj]] -> [[ExpObj]] -> [[ExpObj]]
prdMatrix p e es = map (map (NumO p)) $ M.toList  $ M.transpose $ ( m1) * (M.transpose m2) 
                    where
                        m1 =  M.fromList (map getNums e)
                        m2 =  M.fromList (map getNums es)

                  
prdMatrixF :: ObjPos -> [[ExpObj]] -> [[ExpObj]] -> EvalFunc ExpObj
prdMatrixF p e es = return $ ArrO p $  map (ArrO p) $ prdMatrix p e es

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

validateLength :: ObjPos -> Int -> [a] -> (Pos -> Int -> Int -> EvalError) -> EvalFunc [a]
validateLength p expected val mkError = let actual = length val in if expected == actual then return val else lift $ Left $ mkError (fromObjPos p) expected actual

-- Repeats the given number n times
nTimesF :: ObjPos -> ExpObj -> Double -> EvalFunc ExpObj
nTimesF p v n = return $ ArrO p $ replicate (floor n) v

-- Takes from a table the n first rows
takeTF :: ObjPos -> ObjPos -> Int -> [[ExpObj]] -> Param -> EvalFunc ExpObj
takeTF _ q n _ _ | n <= 0 = lift $ Left $ IllegalTakeTableLength (fromObjPos q) 1 n
takeTF p _ n ess h        = return $ TableO p (map (take n) ess) h

-- Takes from an array the n first elements
takeAF :: ObjPos -> Int -> [ExpObj] -> EvalFunc ExpObj
takeAF  p n es = return $ ArrO p $ take n es

-- Sorts a table by its n'th column
sortTF :: ObjPos -> ObjPos -> Int -> [[ExpObj]] -> Param -> EvalFunc ExpObj
sortTF p pn n ess h = liftM (flip (TableO p) h) $ sortMatrix pn n ess

sortMatrix :: ObjPos -> Int -> [[ExpObj]] -> EvalFunc [[ExpObj]]
sortMatrix pn n ess = validateIndex pn n 0 (length ess - 1) >> return (Data.List.transpose $ map snd $ sort $ zip (toIgnorePos ess !! n) $ Data.List.transpose ess)

toIgnorePos :: [[ExpObj]] -> [[ExpObj]]
toIgnorePos = map $ map $ setPos $ Calc (0,0) where
  setPos p (TableO _ a b) = TableO p a b
  setPos p (PlotO  _ a b) = PlotO  p a b
  setPos p (ArrO _ a)     = ArrO p a
  setPos p (ObjO _ a)     = ObjO p a
  setPos p (StrO _ a)     = StrO p a
  setPos p (NumO _ a)     = NumO p a
  setPos p (BoolO _ a)    = BoolO p a
  setPos p (NullO _)      = NullO p

--Take the mean of he given numbers
fstF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
fstF p n = (validateIndex p 0 0 (length n - 1)) >> (return (n !! 0))

sndF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
sndF p n = (return (n !! 1))



validateIndex :: ObjPos -> Int -> Int -> Int -> EvalFunc ()
validateIndex pn n minL maxL = when (n < minL || n > maxL) $ lift $ Left $ IndexOutOfBounds (fromObjPos pn) minL maxL n

-- Sorts an array of arrays (matrix) by the n'th array-element (column)
sortAF :: ObjPos -> ObjPos -> Int -> [ExpObj] -> EvalFunc ExpObj
sortAF p pn n arrays =  let (fs,ess) = unzip $ map (\(ArrO q es) -> (ArrO q,es)) arrays
                        in  liftM (ArrO p . zipWith ($) fs) (sortMatrix pn n ess)

transF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
transF p arrays  = let (fs,ess) = unzip $ map (\(ArrO q es) -> (ArrO q,es)) arrays
                 in  liftM (ArrO p . zipWith ($) fs) (return $ Data.List.transpose ess) 
-- Extracts from a table the n'th column as an array
colTF :: ObjPos -> ObjPos -> Int -> [[ExpObj]] -> EvalFunc ExpObj
colTF p pn n ess = validateIndex pn n 0 (length ess - 1) >> return (ArrO p $ ess !! n)

-- Extracts from a table the r'th row as an array
rowTF :: ObjPos -> ObjPos -> Int -> [[ExpObj]] -> EvalFunc ExpObj
rowTF p pn n ess = validateIndex pn n 0 (length (ess !! 0) - 1) >> return (ArrO p $ (map (\e -> e !! n) ess))


-- Extracts from an array of arrays (matrix) the n'th array-element (column)
colAF :: ObjPos -> ObjPos -> Int -> [ExpObj] -> EvalFunc ExpObj
colAF p pn n arrays = validateIndex pn n 0 (length arrays - 1) >> return (let ArrO _ es = arrays !! n in ArrO p es)

-- Extracts from an array of arrays (matrix) the n'th array-element (column)
concatF :: ObjPos -> [ExpObj] -> EvalFunc ExpObj
concatF p arrays = do  ess<-getMatrix arrays; return (ArrO p $ concat ess) 

diffF :: ObjPos -> [ExpObj] -> [ExpObj] -> EvalFunc ExpObj
diffF p es1 es2 =  return $ ArrO p $ map (NumO p) $  zipWith (-)  (getNums es1)  (getNums es2)



-- Plots a line graph using the (x,y) points
plotF :: ObjPos -> [ExpObj] -> [ExpObj] -> [(String,ExpObj)] -> EvalFunc ExpObj
plotF p xs ys ps = return $ PlotO p (zip xs ys) $ (Param  $ getAttributes ps)

getAttributes :: [(String,ExpObj)] -> [(String,ExpObj)]
getAttributes ps = get ps "title" ++ get ps "color"

replaceAttributes :: [(String,ExpObj)] -> String -> ExpObj -> [(String,ExpObj)]
replaceAttributes ps s r = map (\(s1,x) -> if (s1==s) then (s1,r) else (s1,x)) ps

getTableAttributes :: [(String,ExpObj)] -> [(String,ExpObj)]
getTableAttributes ps = get ps "title" ++ get ps "col" ++ get ps "spare" ++ get ps "color" ++ get ps "row"

get :: [(String, ExpObj)] -> String -> [(String, ExpObj)]
get ps n = maybeToList $ do r <- lookup n ps; return (n,r)

breakL p (s:b:[]) = return $ WidgetO p "break" $ Param [("text",s),("line",b)] 