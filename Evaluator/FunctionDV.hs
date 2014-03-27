module Evaluator.FunctionDV  where

import Evaluator.DValue
import Evaluator.FunctionExc
import Evaluator.EqParser
import Data.Maybe
import Data.Dynamic
import Data.List
import Control.Applicative
import Statistics.Distribution.Normal as D
import qualified Data.Map as M
import Evaluator.Stats


table :: DValue -> DValue -> DValue
table (DArray x) p = DObj $ [("type",DString "table"),("data",DArray x), ("p",p)]


plotLine :: DValue -> DValue -> DValue -> DValue
plotLine (x) (y) p = DObj $ [("type",DString "graph"),("x",x), ("y",y), ("p",p)]



annuity :: Double ->  Double -> [Double]
annuity i n = take ((round n) :: Int) $ iterate (*(interest)) 1
				where interest = 1/(1+(i/100))


gt (DNum d1) (DNum d2) = if (d1 > d2) then (DNum d1) else DNot
gt (d1) (DArray d2) = mapArray (gt d1) d2  

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

avg :: DValue -> DValue
avg x = DNum $ average $ selectNum x 

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

mean :: [Double]  ->  Double
mean x  = sum (x) / (genericLength (x))

c :: Double -> [Double] -> [Double]
c d ds = (d:ds)




