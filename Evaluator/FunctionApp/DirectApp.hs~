module Evaluator.FunctionApp.DirectApp  where

import Evaluator.DValue
import Evaluator.EqParser
import Control.Applicative
import Control.Monad
import Data.Either
import Evaluator.Stats
listOfFunctionWithTwoArgs = [("addition",\ x y -> n2n (+) x y `mplus` s2s (++) x y),
			     ("nTimes", \x y -> n2i (nTimes) x y)]
listOfFunctionWithOneArgs = [("descriptive",\ x -> ns1i (descriptive) x)]


applyOnTwo :: String -> DValue -> DValue -> EitherDValue
applyOnTwo f x y = case (lookup f listOfFunctionWithTwoArgs) of
				Just g -> g x y  
 			        Nothing -> Left $ "No function found " ++ f

applyOnOne :: String -> DValue -> EitherDValue
applyOnOne f x  = case (lookup f listOfFunctionWithOneArgs) of
				Just g -> g x   
			        Nothing -> Left $ "No function found " ++ f


applyOn :: String -> [DValue] -> EitherDValue
applyOn f (DOrray x:[]) = Right $ DArray $ rights $ map (applyOnOne f) x 
applyOn f (x:[])   = applyOnOne f x
applyOn f (DOrray x:DOrray y:[]) = Right $ DArray $ rights $ zipWith (applyOnTwo f) x y 
applyOn f (DOrray x:y:[]) = Right $ DArray $ rights $ pam (map  (applyOnTwo f) x) y 
applyOn f (x:DOrray y:[]) = Right $ DArray $ rights  $ (map (applyOnTwo f x) y) 
applyOn f (x:y:[]) = applyOnTwo f x y



pam :: [a -> b] -> a -> [b]
pam f x = map g f
  where g h = h x


a1 :: (b -> DValue) ->  (a -> b)  -> Either String a -> EitherDValue
a1 g f a =  g <$> (f <$> a) 

a2 :: (c -> DValue) ->  (a -> b -> c)  -> Either String a -> Either String b  -> EitherDValue
a2 g f a b  = g <$> (f <$> a <*> b) 




ns1i f x =   a1 id f (vNums x)

n1n f x =   a1 DNum f (vNum x)
n1s f x =   a1 DString f (vNum x)



n2n f x y =   a2 DNum f (vNum x) (vNum y) 
n2i f x y =   a2 id f (vNum x) (vNum y) 

s2s f x y=    a2 DString f (vString x) (vString y) 




(*+) :: Double -> Double -> Double
0 *+ y = error "Non"
y *+ x= x*y


test = [("sum",\x y -> (+) (evalNum x) (evalNum y))]


evalNum :: DValue -> Double
evalNum (DNum x) = x
evalNum (DFunction (f,(x1:x2:[]))) = case (lookup f test) of
				Just g -> g x1 x2    
			       
