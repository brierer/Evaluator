module Eval.EngineTestUtils where

import qualified Eval.FunctionEvalTestUtils as FU (w2)

import Control.Applicative                        ((<$>),(<*>))
import Control.Monad                              (liftM)
import Control.Monad.Trans                        (lift)
import Data.Eval                                  (EvalError(..),ExpObj(..),Func(..))
import Data.List                                  (nub)
import Data.Token                                 (PairToken(..),IdToken(..),ExpToken(..))
import Eval.Engine                                (funcs)
import Eval.FunctionEvalTestUtils                 (ExpOA,ExpTS,isTable,isPlot,p0,ws2,applyFunc)
import Parser.MonolithicParserTestUtils           (Tall(..),Unto,to,uns,tShrinks,sListOf,sized1)
import Test.Framework                             (Arbitrary(..),(==>))

fs = flip map funcs $ \(n,(typeValidators,_)) -> (n,(typeValidators, Func $ \_ _ -> lift $ success n))

mk  p ts = let es = uns ts; arg = ArrayT p FU.w2 es in (es,arg)
mkO p ts = let es = uns ts; arg = ArrayO p       es in (es,arg)

{-# ANN mk'  "HLint: ignore Eta reduce" #-}
{-# ANN mkO' "HLint: ignore Eta reduce" #-}
mk'  ts = mk p0 ts
mkO' ts = mkO p0 ts

oneArrayOfNum g1ras w1as = let es = uns g1ras; arg = ArrayO p0 es; (xs,g1) = addFunc "arrayOfNum" arg; (w1s,w1) = mk' w1as in (xs,g1,w1s,w1)

addFunc  fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):fs;    g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)
addFunc' fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):funcs; g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)

success n = return $ StrO p0 $ "Mocked Success ["++n++"]"

toArray (x,y) e = ArrayT p0 FU.w2 $ replicate ((x+y) `mod` 100) e

tablesAndPlots :: [ExpOA] -> [ExpOA]
tablesAndPlots xs = let ys = uns xs :: [ExpObj] in  map to (filter ((||) <$> isTable <*> isPlot) ys)

emptyArray (ArrayT _ _ es) = null es
emptyArray e               = error $ "Eval.EngineTestUtils::emptyArray [Unexpected pattern ["++show e++"]]" 

emptySortColCase name pa pt n g2ass w2'ass = 
  let (g2ss,g2) = mk' g2ass; (_,w2) = mk pa ([] :: [ExpTS]); (_,w2') = mk' (w2'ass ++ [to w2]); in any (not.emptyArray) g2ss ==>
    Left (IllegalEmpty pa) == applyFunc fs pt name [n, w2 ] &&
    Left (IllegalEmpty pa) == applyFunc fs pt name [n, w2'] &&
    success name          == applyFunc fs pt name [n, g2 ]

tableColumsLengthCase w1ps a2 = 
  let arrays = map (\(p,es)-> (p,ArrayT p ws2 es)) w1ps; ls = map (length.snd) w1ps; l = head ls in any (not.null.snd) w1ps ==> 
  case applyFunc funcs p0 "table" [ArrayT p0 ws2 $ map snd arrays,a2] of
    Left (TableHeaderLengthMismatch p expected actual) -> let Just (ArrayT pa _ es) = lookup p arrays in pa == p && l == expected && length es == actual
    Right _                                            -> [l] == nub ls
    e                                                  -> error $ "EngineTestUtils::tableColsCase [Unexpected pattern ["++show e++"]]"
    
tableHeaderLengthCase pf g1ss g2s = 
  let g1 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) $ equalize g1ss
      w2 = ObjT   p0 ws2 [PairT (IdT p0 ws2 "col") $ ArrayT p0 ws2 g2s]
      (l1,l2) = (length g1ss,length g2s)
  in any (not.null) g1ss && not (null g2s) && l1 `notElem` [0,l2] ==> 
  Left (TableHeaderLengthMismatch pf l1 l2) == applyFunc funcs pf "table" [g1,w2] 
    
equalize g1ss = map (take l) g1ss where l = minimum $ map length g1ss
    
mkMultiMeanReturn a1as pa = let a1s = uns a1as; a1rs = map (\(NumT q _ _ x) -> NumO q x) a1s; a1 = ArrayT pa ("","") a1s; a1r = ArrayO pa a1rs in (a1,a1rs,a1r)

unprecise :: Monad m => m ExpObj -> m ExpObj
unprecise = liftM moo where 
  moo (NumO p x) = NumO p $ fromIntegral (floor $ 1000000.0 * x :: Integer) / 1000000.0
  moo e          = error $ "Eval.EngineTestUtils::unprecise [Unexpected pattern ["++show e++"]]"
  
data TableValidArgs = TableValidArgs [[ExpToken]] [ExpToken] deriving (Show)
instance Arbitrary TableValidArgs where arbitrary = sized1 tall; shrink (TableValidArgs ess es) = mTableValidArgs (shrink $ map (map to) ess)  (tShrinks es)
instance Tall      TableValidArgs where                                                  tall n = mTableValidArgs (sListOf $ sListOf $ tall n) (sListOf $ tall n)
mTableValidArgs tssa tsa = do tss <- tssa; ts <- tsa; let l = minimum [length tss, length ts] in return $ TableValidArgs (equalize $ map uns $ take l tss) $ uns $ take l ts 

mkTableValidArgs pf g1ss g2s useHeader = 
  let expectedHeader = concat [map unsafeMarshall g2s                       | useHeader]
      inputHeader    =        [PairT (IdT p0 ws2 "col") $ ArrayT p0 ws2 g2s | useHeader] 
      g1 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1ss
      g2 = ObjT p0 ws2 inputHeader  
      expected       = Right (TableO pf (map (map unsafeMarshall) g1ss) expectedHeader)
  in (g1,g2,expected)

unsafeMarshall :: ExpToken -> ExpObj  
unsafeMarshall = undefined
  
{-| Monomorphism restriction -}
mTableValidArgs :: Monad m => m [[ExpTS]] -> m [ExpTS] -> m TableValidArgs 
  