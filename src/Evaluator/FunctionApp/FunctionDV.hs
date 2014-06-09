module Evaluator.FunctionApp.FunctionDV  where

import Evaluator.DValue
import Evaluator.EqParser
import Data.List
import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Maybe
import Statistics.Distribution.Normal as D
import qualified Data.Map as M
------------------------------------------


findFuncNative :: String -> (Dynamic, String)
findFuncNative s = fromJust $ lookup s functionNative -- bug

findFunc :: String -> (Dynamic, String)
findFunc s = fromJust $ lookup s function' -- bug

isSemiDirectFunction :: String -> Bool
isSemiDirectFunction f = case  (lookup f function') of
                                                Nothing -> False
                                                _ -> True
-----------------------------------------------


functionNative = [
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

function' = [ ("mean", (toDyn (mean), show $ typeOf(mean))),
             ("table", (toDyn table, show $ typeOf(table))),
             ("plotLine", (toDyn plotLine, show $ typeOf(plotLine))),
             ("gt", (toDyn gt, show $ typeOf(gt))),
               ("sort", (toDyn sortD, show $ typeOf(sortD))),
              ("sortTable", (toDyn sortTable, show $ typeOf(sortTable))),
             ("take", (toDyn takeD, show $ typeOf(takeD))),
               ("avg", (toDyn avg, show $ typeOf(avg))),
               ("col", (toDyn col, show $ typeOf(col))),
             ("o", (toDyn o, show $ typeOf(o)))
           ]

---------------------------------------------------------------

o :: DValue  -> DValue
o d = DOrray d 


table :: DValue -> DValue -> DValue
table x@(DArray xs) p = DObj $ [("type",DString "table"),("data",convertTable x), ("p",p)]


plotLine :: DValue -> DValue -> DValue -> DValue
plotLine (x) (y) p = DObj $ [("type",DString "graph"),("x",x), ("y",y), ("p",p)]



annuity :: Double ->  Double -> [Double]
annuity i n = take ((round n) :: Int) $ iterate (*(interest)) 1
                                where interest = 1/(1+(i/100))


gt (DNum d1) (DNum d2) = if (d1 > d2) then (DNum d1) else DNot


sortD :: DValue -> DValue
sortD (DArray ds) = DArray $ reverse $ sort ds 


sortTable :: DValue -> DValue -> DValue
sortTable (DNum d )(DArray ds) = DArray $ map DArray $ tail $ transpose $ reverse $ sort $ transpose (map isArray $ putFirst (floor d) ds)


putFirst :: Int -> [DValue] -> [DValue]
putFirst x xs = [(xs !! x)] ++ xs


--selectCol :: DValue -> [DValue] -> [DValue]
--selectCol (DNum d) (ds) = isArray (ds !! (floor d))

isArray :: DValue -> [DValue]
isArray (DArray ds) = ds   

col :: DValue -> DValue -> DValue
col (DNum d) (DArray ds) = ds !! (floor d)

takeD :: DValue -> DValue -> DValue
takeD (DNum d) (DArray ds) = DArray $ map DArray $ map (take $ floor d) $  map  isArray ds  
 

mapArray :: (DValue -> DValue) -> [DValue] -> DValue
mapArray f (ds) = DArray $ map f ds 

selectNum :: DValue -> [Double]
selectNum (DArray (x:[]))  = (if (isNum x) then [fromNum x] else []) 
selectNum (DArray (x:xs))  = (if (isNum x) then [fromNum x] else [])  ++ (selectNum $ DArray xs) 



isNum:: DValue -> Bool
isNum (DNum x) = True
isNum _ = False

avg :: DValue -> DValue -> DValue
avg (DNum x)  (DNum y) = DNum $ x + y  

---------------------------------------
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

mean :: DValue  ->  DValue
mean a@(DArray xs)  = DNum $ sum (x) / (genericLength (x))
                        where x= selectNum a 

c :: Double -> [Double] -> [Double]
c d ds = (d:ds)



