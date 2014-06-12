{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Evaluator.EvalParse
 where

import           Control.Applicative
import           Control.Exception                   as Except
import           Control.Monad.Error
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Aeson                          (FromJSON, ToJSON, decode,
                                                      encode)
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8          (unpack)
import           Data.Dynamic
import           Data.Either
import qualified Data.Map                            as M
import           Data.Maybe
import           Evaluator.DValue
import           Evaluator.EqParser
import           Evaluator.FunctionApp.DirectApp
import           Evaluator.FunctionApp.DynamicApp
import           Evaluator.FunctionApp.FunctionDV
import           Evaluator.FunctionApp.SemiDirectApp
import           System.Environment
import           Control.Monad.State

type EvaluatedValue = ErrorT String (Writer [StackInfo]) DValue
type EvaluatedValues = ErrorT String (Writer [StackInfo]) [DValue]

data StackInfo = StackInfo (StackInfoType,String)
data StackInfoType = Function | Array | Argument | Equation | Bool | Number | Str deriving ( Show )

data EvalStatut = GoodEval | BadEval | ErrorEval
data EvalResult = EvalResult EvalStatut String String

instance Show StackInfo where
        show (StackInfo (info,msg)) = "{" ++ (show $ show info) ++ ":" ++ show msg ++ "}"

instance Show EvalStatut where
        show GoodEval = "\"ok\""
        show BadEval = "\"tko\""
        show ErrorEval = "\"ko\""


instance Show EvalResult where
        show (EvalResult statut res stack) = "{\"statut\":" ++ show statut ++ ",\"res\":" ++ res ++ ",\"stack\":" ++ stack ++ "}"


runParse s = case  (run equations s) of
                Right x -> do
                           result <- evalParse $ convertAllToPureValue x
                           return $ "{\"parse\":" ++ (unpack $ (encodeValues x)) ++ ",\"eval\":" ++ result ++ "}"
                Left  x -> return $ show $ x

evalParse :: [(String,Pvalue)] -> IO String
evalParse x = do
                putStrLn "Start Eval:\n"
                let res =  runWriter $ runErrorT $ evalEqs $ (x)
                Except.catch (case res of
                               (Right x,w) -> goodEval x w
                               (Left  x,w) -> badEval x w
                              ) (errorEval $ snd $ res)

goodEval :: (Show a, Show w) => a -> w -> IO String
goodEval a w  =do
                let s = show $  EvalResult GoodEval (show a) (show w)
                putStrLn $ s
                return $ s


badEval :: (Show a, Show w) =>   a -> w -> IO String
badEval a w  = do
               let s =  show $ EvalResult BadEval (show a) (show w)
               putStrLn s
               return $ s

errorEval :: Show w =>  [w] -> Except.SomeException -> IO String
errorEval w e=  do
                s <- (safePrint $ w)
                return $ show $  EvalResult ErrorEval (show $ "Error") ("[" ++ s ++ "]")


safePrint :: Show w => [w] -> IO String
safePrint (s:[]) = do
                   putStrLn $ show s
                   Except.catch (return $ show s) (\(x :: SomeException) -> return "{\"err\":\"Error\"}")
safePrint (s:ss) = do
                   putStrLn $ show s
                   one <-  Except.catch (return $ show s) (\(x :: SomeException) -> return "{\"err\":\"Error\"}") --
                   many <- Except.catch (safePrint ss) (\(x :: SomeException) -> return "{\"err\":\"Error\"}")
                   return $ one ++ "," ++ many


---------------------------------
type StateValue = State (M.Map String (Either Pvalue EvaluatedValue)) EvaluatedValue
type StateValues = State (M.Map String (Either Pvalue EvaluatedValue)) [EvaluatedValue]
type ObjValues = State (M.Map String (Either Pvalue EvaluatedValue)) ( ErrorT String (Writer [StackInfo]) [(String,DValue)])
evalEqs :: [(String, Pvalue)] -> EvaluatedValue
evalEqs (d) = evalState (evalEq (head d)) (M.fromList $ map (mapSnd $ Left ) d) 

evalEq :: (String, Pvalue) -> StateValue
evalEq  (s, ds ) = do 
                  x <- evalOne ds
                  let f _ = Just $ Right $ x 
                  modify ( M.update (f) s )    
                  return $ tell [StackInfo (Equation,s)] >> x


evalOne :: Pvalue -> StateValue
evalOne (Pfunction (d)) = evalFunction d
evalOne (Parray ds)  = evalArr ds
evalOne (Pobj ds)  = evalObj ds
evalOne p =do
             --tell [StackInfo (Number,show p)]
             return $ ErrorT (return (Right $ p_to_Dvalue p))


evalObj ::   [(String,Pvalue)] -> StateValue
evalObj (ds) = do
                    x <- (evalMany (map snd ds))
                    return $ do
                            y <- (sequence x)
                            return $ DObj $ (zip (map fst ds)) y


evalFunction ::   (String, [Pvalue]) -> StateValue
evalFunction (f,[])  = do
                       x <- findEq f
                       return $ tell [StackInfo (Equation , f ++ "")] >> x 
evalFunction (f,ds)  =  do
                        x <- evalMany ds
                        return $
                                 do
                                 y <-  sequence x
                                 res <- ErrorT $ return $ if   isSemiDirectFunction f
                                                          then applyToDValue (findFunc f) y
                                                          else applyOn  f y
                                 return  $ res

findEq :: String -> StateValue
findEq s  = do
            x <- get
            let e = M.lookup s x
            let f = \p -> evalEq (s,p)
            either (f) (return.id) (fromJust e)


--evalObj :: [(String, Pvalue)] -> StateValue
--evalObj d = do 
 --             x <- evalObjTuples
   --           return $ DObj <$> x  

evalMany ::  [Pvalue] -> StateValues
evalMany d = sequence $ map evalOne d

evalArr :: [Pvalue]  -> StateValue
evalArr d  = do 
            x  <- evalMany d
            return $ tell [StackInfo (Array,"[")] >> DArray <$> sequence x

p_to_Dvalue :: Pvalue -> DValue
p_to_Dvalue (Pnum x) = DNum x
p_to_Dvalue (Pstring x) = DString x
p_to_Dvalue (Pbool x) = DBool x