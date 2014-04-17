module Evaluator.FunctionApp.DirectApp  where

import Evaluator.DValue
import Evaluator.EqParser
import Control.Applicative
import Control.Monad
import Data.Either
import Evaluator.Stats
listOfFunctionWithTwoArgs = [("addition",\ x y -> n2n (+) x y),
			     ("nTimes", \x y -> n2i (nTimes) x y)]
listOfFunctionWithOneArgs = [("descriptive",\ x -> ns1i (descriptive) x)
			     ,("plustwo",n1n (+2) )]


applyOnTwo :: String -> DValue -> DValue -> EitherDValue
applyOnTwo f x y = case (lookup f listOfFunctionWithTwoArgs) of
				Just g -> g x y  
 			        Nothing -> Left $ "No function found " ++ f

applyOnOne :: String -> DValue -> EitherDValue
applyOnOne f x  = case (lookup f listOfFunctionWithOneArgs) of
				Just g -> g x   
			        Nothing -> Left $ "No function found " ++ f


applyOn :: String -> [DValue] -> EitherDValue
--applyOn f a@(DOrray x:[]) = applyOnOpenArray f a 
applyOn f (x:[])   = applyOnOne f x
--applyOn f a@(DOrray x:y:[]) = applyOnOpenArray f a 
--applyOn f a@(x:DOrray y:[]) = applyOnOpenArray f a 
applyOn f (x:y:[]) = applyOnTwo f x y


--applyOnOpenArray :: String -> [DValue] -> EitherDValue
--applyOnOpenArray f a@(DOrray (DArray x):[]) = Right $ DArray $ rights $ map (applyOnOne f) x 
--applyOnOpenArray f a@(DOrray (DArray x):DOrray (DArray y):[]) = Right $ DArray $ rights $ zipWith (applyOnTwo f) x y 
--applyOnOpenArray f a@(DOrray (DArray x):y:[]) = Right $ DArray $ rights $ pam (map  (applyOnTwo f) x) y 
--applyOnOpenArray f a@(x:DOrray (DArray y):[]) = Right $ DArray $ rights  $ (map (applyOnTwo f x) y) 

pam :: [a -> b] -> a -> [b]
pam f x = map g f
  where g h = h x
        

a1 :: (b -> DValue) ->  (a -> b)  -> Either String a -> EitherDValue
a1 g f a =  g <$> (f <$> a) 

a2 :: (c -> DValue) ->  (a -> b -> c)  -> Either String a -> Either String b  -> EitherDValue
a2 g f a b  = g <$> (f <$> a <*> b) 




ns1i f x =   a1 id f (vNums x)

n1n f (DOrray (x)) = DNums <$> map f <$> (vNums x)
n1n f x =    DNum <$> f <$> (vNum x)


n1s f x =   a1 DString f (vNum x)

ns2 f (x) (y) = xx2 f x y vNum vNums

xx2 f (DOrray x) (DOrray y) g gs =  (zipWith f) <$> (gs x) <*> (gs y) 
xx2 f (DOrray x) (y) g gs =   ((pam) <$> (map f <$> gs x) <*> (g y)) 
xx2 f (x) (DOrray y) g gs =  (map <$> (f <$> (g x)) <*> (gs y)) 



n2 f x y = f <$> (vNum x) <*> (vNum y)
--n2 f x y = (map <$> (f <$> (vNum x)) <*> (vNums y)) 


n2n f a@(DOrray x) (y) =   DNums <$> ns2 f a y
n2n f (x) a@(DOrray y) =   DNums <$> ns2 f x a
n2n f x y =  DNum <$> n2 f x y


n2s f a@(DOrray x) (y) =   DStrings <$> ns2 f x a
n2s f (x) a@(DOrray y) =   DStrings <$> ns2 f x a
n2s f x y =  DString <$> n2 f x y
 
n2i f x y =   a2 id f (vNum x) (vNum y) 



s2s f x y=    a2 DString f (vString x) (vString y) 


