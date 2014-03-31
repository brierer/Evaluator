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
import Control.Monad
import Data.Either

listOfFunctionWithTwoArgs = [("addition",\ x y -> n2n (+) x y `mplus` s2s (++) x y)]
listOfFunctionWithOneArgs = [("plus2",\ x -> n1n (+2) x)]


applyOnTwo :: String -> DValue -> DValue -> Either String DValue
applyOnTwo f x y = case (lookup f listOfFunctionWithTwoArgs) of
				Just g -> g x y  
 			        Nothing -> Left $ "No function found" ++ f

applyOnOne :: String -> DValue -> Either String DValue
applyOnOne f x  = case (lookup f listOfFunctionWithOneArgs) of
				Just g -> g x   
			        Nothing -> Left $ "No function found" ++ f


applyOn :: String -> [DValue] -> Either String DValue
applyOn f (DArray x:[]) = Right $ DArray $ rights $ map (applyOnOne f) x 
applyOn f (x:[])   = applyOnOne f x
applyOn f (DArray x:DArray y:[]) = Right $ DArray $ rights $ zipWith (applyOnTwo f) x y 
applyOn f (DArray x:y:[]) = Right $ DArray $ rights $ pam (map  (applyOnTwo f) x) y 
applyOn f (x:DArray y:[]) = Right $ DArray $ rights  $ (map (applyOnTwo f x) y) 
applyOn f (x:y:[]) = applyOnTwo f x y



pam :: [a -> b] -> a -> [b]
pam f x = map g f
  where g h = h x


a1 :: (b -> DValue) ->  (a -> b)  -> Either String a -> Either String DValue
a1 g f a = g <$> (f <$> a) 

a2 :: (c -> DValue) ->  (a -> b -> c)  -> Either String a -> Either String b  -> Either String DValue
a2 g f a b  = g <$> (f <$> a <*> b) 


n1n f x =   a1 DNum f (vNum x)
n1s f x =   a1 DString f (vNum x)

n2n f x y =   a2 DNum f (vNum x) (vNum y) 
s2s f x y=    a2 DString f (vString x) (vString y) 

