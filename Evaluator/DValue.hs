{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}


module Evaluator.DValue     
 where


import Data.Dynamic
import Data.Maybe
import Control.Applicative



data DValue = DArray [DValue] |  DOrray [DValue] | DString String | DNum Double | DBool Bool | DCom String | DFunction (String,[DValue]) | DObj [(String, DValue)] | Dyn Dynamic | DNot  deriving (Typeable)

data Tvalue = Arr | Num | Str | Boo | Obj 

data DState = DState {
      dvalue :: DValue
    , string :: String
    } deriving (Show)   

type EitherDValue = Either String DValue
type EitherDValues = Either String [DValue]



commentStack :: String -> DState -> DState
commentStack s d = DState (dvalue d) (s ++ string d)

--newtype Eval a = Eval {
  --    runEval :: DState -> Either String (a, DState)
    --}


---putState :: DState -> Eval ()
---putState s = Eval (\_ -> Right ((), s))

{-
identity :: a -> Eval a
identity a = Eval (\s -> Right (a, s))


(==>) :: Eval a -> (a -> Eval b) -> Eval b
firstEval ==> secondEval  =  Eval chainedEval
  where chainedEval initState   =
          case runEval firstEval initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runEval (secondEval firstResult) newState

-}
--instance Monad Eval where
--    return = identity
 --   (>>=) = (==>)

instance Eq DValue where
	(==) (DNum d1) (DNum d2) = d1 == d2

	
instance Ord DValue where
	(>=) (DNum d1) (DNum d2) = d1 >= d2
	(>) (DNum d1) (DNum d2) = d1 > d2
	(<) (DNum d1) (DNum d2) = d2 >= d1
	(<=) (DNum d1) (DNum d2) = d2 > d1
	compare (DNum d1) (DNum d2) = compare  d1  d2
	compare x (DNot) = GT
	compare (DNot) (x) = LT
	compare _ _ = EQ

instance Show DValue where
	show (DNum d) = show d
	show (DString d) = show  d
	show (DArray []) = "[]" 
	show (DArray d) = "[" ++ foldr (++) [] ((show $ head $ d) : fmap (("," ++) . show) (tail d)) ++ "]"
	show (DOrray x) = show $ DArray x
	show (Dyn d) = show d	
	show (DNot)  = show ""
	show (DObj ds)  = "{" ++ printTag ds ++ "}"

 	

printTag :: [(String, DValue)] -> String
printTag  [] = ""
printTag (d:[]) = (show $ fst d) ++ ":" ++ (show $ snd d) 
printTag (d:ds) = (show $ fst d) ++ ":" ++ (show $ snd d)   ++ "," ++ printTag ds



fromDValue :: DValue -> Dynamic 
fromDValue (DNum d ) = toDyn (d)
fromDValue (x) = toDyn $ (read $ (show x) :: [Double]) 

fromNum ::  DValue -> Double
fromNum (DNum d) = d 

fromString :: DValue -> String
fromString (DString x) = x

vNum :: DValue -> Either String Double
vNum (DNum d) = Right d
vNum d =	Left $ "Bad type" 

vNums :: DValue -> Either String [Double]
vNums (DArray ds) = mapM vNum ds  
 
vString :: DValue -> Either String String
vString (DString d) = Right d
vString _ =	Left "Bad type"

convertListOfDState :: Either String [DState] -> Either String [DValue]
convertListOfDState ds = map dvalue <$> ds 
 
convertTupleDState :: [(String,DState)] -> [(String,DValue)]
convertTupleDState (d:[]) = [(fst d, dvalue $ snd d)]
convertTupleDState (d:ds) = (fst d, dvalue $ snd d) : convertTupleDState ds



concatStringTuple :: Either String [(String, DState)]  -> String
concatStringTuple (Right ds) = foldr (++) "" (map (string.snd) ds)
concatStringTuple _ = ""


concatString :: Either String [DState]  -> String
concatString (Right ds) = foldr (++) "" (map string ds)
concatString  _ = ""

