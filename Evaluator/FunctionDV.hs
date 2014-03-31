module Evaluator.FunctionDV  where

import Evaluator.DValue
import Evaluator.EqParser
import Data.List
import Control.Applicative
import Control.Monad

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

mean :: [Double]  ->  Double
mean x  = sum (x) / (genericLength (x))

c :: Double -> [Double] -> [Double]
c d ds = (d:ds)



----------------------




plus2 :: DValue -> DValue  -> Either String DValue
plus2 x y = do 
	    x1 <- vNum x
	    x2 <- vNum y
	    Right $ DNum $ ((+) x1) x2  

app (x,y) = appPlus x y 

appPlus x y =  applyTwo DNum (+) (vNum x) (vNum y) `mplus` 
	       applyTwo DString (++) (vString x) (vString y)


n3 =  \f -> appTwoLambda DNum f vNum vNum
s3 =  \f -> appTwoLambda DString f vString vString



pam :: [a -> b] -> a -> [b]
pam f x = map g f
  where g h = h x

--orr :: Either String DValue -> Either String DValue -> Either String DValue
--orr (Right x) _ = Right x
--orr _  x = x  


appTwoLambda :: (c -> DValue) ->  (a -> b -> c)  -> (DValue -> Either String a) -> (DValue -> Either String b)  -> (DValue -> DValue -> Either String DValue)
appTwoLambda g f a b = (\x y -> applyTwo g f (a x) (b y)) 

applyTwo :: (c -> DValue) ->  (a -> b -> c)  -> Either String a -> Either String b  -> Either String DValue
applyTwo g f a b  = g <$> (f <$> a <*> b) 



