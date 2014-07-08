module Eval.Engine
( funcs
, showF
, multiF
, meanF
, descF
, tableF
, nTimesF
, takeF
, sortF
, plotLineF
) where

import Prelude hiding    (sum,exp,null)

import qualified Prelude as P (sum)

import Data.Eval         (EvalError(..),ExpObj(..),EvalFunc,FuncEntry,Func(..))
import Data.Token        (Pos)
import Data.Vector       (Vector,fromList
--,toList,length
  )
import Eval.Function     (table,plot,array,str,num,arrayOf,objOf,nonEmpty,(<|>),evalError)
import Statistics.Sample (mean
--,variance,skewness,kurtosis
  )

funcs :: [FuncEntry]
funcs = -- 1 arg functions
        [ ("show",       ([arrayOf $ table <|> plot                               ], Func showF))
        , ("multi",      ([nonEmpty $ arrayOf num                                 ], Func multiF))
        , ("mean",       ([nonEmpty $ arrayOf num                                 ], Func meanF))
        , ("descriptive",([nonEmpty $ arrayOf num                                 ], Func descF))
          -- 2 arg functions                                                                  
        , ("table",      ([tableArg                , objOf $ arrayOf str          ], Func tableF))
        , ("nTimes",     ([num                     , num                          ], Func nTimesF))
        , ("take",       ([num                     , table <|> array              ], Func takeF))
        , ("sort",       ([num                     , table <|> tableArg           ], Func sortF))
        , ("col",        ([num                     , table <|> tableArg           ], Func colF))
          -- 3 arg functions                       
        , ("plotLine",   ([arrayOf num             , arrayOf num       , objOf str], Func plotLineF))
        ] where tableArg = nonEmpty $ arrayOf $ nonEmpty array 

{-| Funcs -}
showF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
multiF     :: Pos -> [ExpObj] -> EvalFunc ExpObj 
meanF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
descF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
tableF     :: Pos -> [ExpObj] -> EvalFunc ExpObj 
nTimesF    :: Pos -> [ExpObj] -> EvalFunc ExpObj 
takeF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
sortF      :: Pos -> [ExpObj] -> EvalFunc ExpObj 
colF       :: Pos -> [ExpObj] -> EvalFunc ExpObj 
plotLineF  :: Pos -> [ExpObj] -> EvalFunc ExpObj 


showF  p [x]           = return $ ObjO p [("result",x)];                showF  _ xs = error $ "Engine::showF  [Unexpected pattern ["++show xs++"]]"
multiF p [ArrayO _ ns] = return $ NumO p $ product $ getNums ns;        multiF _ xs = error $ "Engine::multiF [Unexpected pattern ["++show xs++"]]"
meanF  p [ArrayO _ ns] = return $ NumO p $ mean $ toStatList ns;        meanF  _ xs = error $ "Engine::meanF  [Unexpected pattern ["++show xs++"]]"
descF        = error "Eval.Function::descF      [Not Implemented]"--p [ArrayO _ ns] = tableF p [mkDescArg1 p ns,ObjO p []];   descF  _ xs = error $ "Engine::descF  [Unexpected pattern ["++show xs++"]]"

tableF _{-p-} [ArrayO _ es, _{-opts-}] = do
  _{-ess-} <- getMatrix es
--  _{-h-} <- getHeader opts
  error "Engine;:tableF [Rest of function not implemented]"

tableF _ xs = error $ "Engine::taleF  [Unexpected pattern ["++show xs++"]]"

getMatrix :: [ExpObj] -> EvalFunc [[ExpObj]]
getMatrix es@(ArrayO _ xs:_) = let l = length xs in mapM (getColumn l) es
getMatrix xs = error $ "Engine::getMatrix [Unexpected pattern ["++show xs++"]]"

getColumn :: Int -> ExpObj -> EvalFunc [ExpObj]
getColumn l (ArrayO p es) = let l' = length es in if l == l' then return es else evalError $ TableColumnLengthMismatch p l l'
getColumn _ x = error $ "Engine::getColumn [Unexpected pattern ["++show x++"]]"

--getHeader :: ExpObj -> EvalFunc (Maybe [ExpObj])
--getHeader _ = return Nothing

nTimesF      = error "Eval.Function::ntimesF    [Not Implemented]"
takeF        = error "Eval.Function::takeF      [Not Implemented]"
sortF        = error "Eval.Function::sortF      [Not Implemented]"
colF         = error "Eval.Function::colF       [Not Implemented]"
plotLineF    = error "Eval.Function::plotLineF  [Not Implemented]"

{-| Utils -}
toStatList :: [ExpObj] -> Vector Double
toStatList = fromList.getNums

getNums :: [ExpObj] -> [Double]
getNums = map (\(NumO _ x)->x)

--count :: Vector a -> Double
--count = fromIntegral . Data.Vector.length
--
--sum :: Vector Double -> Double
--sum = P.sum.toList
--
--mkDescArg1 :: Pos -> [ExpObj] -> ExpObj
--mkDescArg1 p ns = ArrayO p [ArrayO p $ map (StrO p.fst) desc, ArrayO p $ map (NumO p.snd) desc ]
--  where desc = zip ["count","sum","mean","variance","skewness","kurtosis"] $ map ($ toStatList ns) 
--                   [ count , sum , mean , variance , skewness , kurtosis]
      
      

        