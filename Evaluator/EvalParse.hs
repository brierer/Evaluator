module Evaluator.EvalParse(
   evalParse,
   evalParse1,
   evalOne,
   evalArr,
   table,
   mean, 
 ) where

import Evaluator.DValeur
import Evaluator.FunctionExc
import Evaluator.MyParse
import Data.Maybe
import Data.Dynamic
import Data.List
import Control.Applicative
import Statistics.Distribution.Normal as D
import qualified Data.Map as M



evalParse :: String -> String
evalParse s = case  (run bloc s) of
				Right x ->
				 case snd (evalEqs $ (x)) of
					Right y -> show y
					Left  y  ->  y
				Left x  -> show x 			

evalParse1 :: String -> String
evalParse1 s = case  (run bloc s) of
				Right x -> fst (evalEqs  $ (x)) 
				Left  x -> show x

evalEqs :: [(String, DValeur)] -> (String, Either String DValeur) 
evalEqs (d) = evalEq (M.fromList d) (head d)

evalEq :: M.Map String DValeur -> (String, DValeur) -> (String, Either String DValeur) 
evalEq  m (s, ds ) = (s, evalOne m ds)

evalOne :: M.Map String DValeur ->  DValeur -> Either String DValeur
evalOne m (DFunction (d,ds)) = evalFunction (d) (ds) m
evalOne m (DArray ds)  = evalArr m ds 
evalOne m d       = Right d  

evalArg :: M.Map String DValeur ->  [DValeur]  -> Either String [DValeur]
evalArg  m (d) =  mapM (evalOne m) d

evalArr :: M.Map String DValeur -> [DValeur]  -> Either String DValeur
evalArr m (d)  =  DArray <$> mapM (evalOne m)  d


evalFunction ::   String -> [DValeur] -> M.Map String DValeur -> Either String DValeur
evalFunction f [] m = findEq f m
evalFunction f ds m = if  (isNativeFunction f)
					  then (execFunc $ findFuncNative f) =<< (evalArg m ds) 
					  else (applyToDValeur $ findFunc f) =<< (evalArg m ds) 

findEq :: String  ->  M.Map String DValeur  -> Either String DValeur
findEq s m = (evalOne m $ fromJust (M.lookup s m))

findFuncNative :: String -> (Dynamic, String)
findFuncNative s = fromJust $ lookup s functionNative -- bug

findFunc :: String -> (Dynamic, String)
findFunc s = fromJust $ lookup s function -- bug

isNativeFunction :: String -> Bool
isNativeFunction f = case  (lookup f functionNative) of
						Nothing -> False
						_ -> True



functionNative = [
			 ("mean", (toDyn (mean), show $ typeOf(mean))),
			 ("normalDistr", (toDyn D.normalDistr, show $ typeOf(D.normalDistr))) ,
			 ("annuity", (toDyn annuity, show $ typeOf(annuity))),
			 ("add", (toDyn add, show $ typeOf(add))),
			 ("sum", (toDyn addV, show $ typeOf(addV))),
 			 ("sums", (toDyn sum2, show $ typeOf(sum2))),
			 ("multi", (toDyn multiplyV, show $ typeOf(multiplyV))),
			 ("slide", (toDyn slide, show $ typeOf(slide))),
			 ("nTimes", (toDyn nTimes, show $ typeOf(nTimes))),
			 ("c", (toDyn c, show $ typeOf(c)))
			]

function = [ ("table", (toDyn table, show $ typeOf(table))),
	     ("plotLine", (toDyn plotLine, show $ typeOf(plotLine))),
	     ("gt", (toDyn gt, show $ typeOf(gt))),
  	     ("sort", (toDyn sortD, show $ typeOf(sortD))),
 	     ("sortTable", (toDyn sortTable, show $ typeOf(sortTable))),
	     ("take", (toDyn takeD, show $ typeOf(takeD))),
  	     ("col", (toDyn col, show $ typeOf(col)))
	   ]

table :: DValeur -> DValeur
table (DArray x) = DString $ show $  x


plotLine :: DValeur -> DValeur
plotLine (x) = DString $ "{\"type\":\"graph\",\"data\":" ++ ( show $  x) ++ "}"


annuity :: Double ->  Double -> [Double]
annuity i n = take ((round n) :: Int) $ iterate (*(interest)) 1
				where interest = 1/(1+(i/100))

gt :: DValeur -> DValeur -> DValeur
gt (DNum d1) (DNum d2) = if (d1 > d2) then (DNum d1) else DNot
gt (d1) (DArray d2) = mapArray (gt d1) d2  

sortD :: DValeur -> DValeur
sortD (DArray ds) = DArray $ reverse $ sort ds 


sortTable :: DValeur -> DValeur -> DValeur
sortTable (DNum d )(DArray ds) = DArray $ map DArray $ tail $ transpose $ reverse $ sort $ transpose (map isArray $ putFirst (floor d) ds)


putFirst :: Int -> [DValeur] -> [DValeur]
putFirst x xs = [(xs !! x)] ++ xs


--selectCol :: DValeur -> [DValeur] -> [DValeur]
--selectCol (DNum d) (ds) = isArray (ds !! (floor d))

isArray :: DValeur -> [DValeur]
isArray (DArray ds) = ds   

col :: DValeur -> DValeur -> DValeur
col (DNum d) (DArray ds) = ds !! (floor d)

takeD :: DValeur -> DValeur -> DValeur
takeD (DNum d) (DArray ds) = DArray $ map DArray $ map (take $ floor d) $  map  isArray ds  
 

mapArray :: (DValeur -> DValeur) -> [DValeur] -> DValeur
mapArray f (ds) = DArray $ map f ds 

nTimes :: Double -> Double -> [Double]
nTimes n t = replicate ((round n) :: Int) t

multiplyV :: [Double] -> [Double] -> [Double]
multiplyV v b = zipWith (*) v b

slide :: Double -> Double -> Double -> [Double]
slide a b n = enumFromThenTo a b (a+(b-a)*n) 
 
addV :: [Double] -> Double
addV a = sum a

sum2 :: Double -> Double
sum2 a = a + 2

add  ::  Double -> Double -> Double
add a b = (a + b)  

mean :: [Double]  ->  Double
mean x  = sum (x) / (genericLength (x))

c :: Double -> [Double] -> [Double]
c d ds = (d:ds)
