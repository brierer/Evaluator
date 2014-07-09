module Eval.EngineTestUtils where

import qualified Eval.FunctionEvalTestUtils1 as FU (w2)

import Control.Applicative                         ((<$>),(<*>))
import Control.Monad                               (liftM)
import Control.Monad.Trans                         (lift)
import Data.Eval                                   (EvalError(..),ExpObj(..),Func(..))
import Data.List                                   (sort,transpose)
import Data.Token                                  (PairToken(..),IdToken(..),ExpToken(..))
import Eval.Engine                                 (funcs)
import Eval.FunctionEvalTestUtils1                 (ExpOA,ExpTS,p0,ws2,applyFunc)
import Eval.FunctionEvalTestUtils2                 (elems,isTable,isPlot)
import Parser.MonolithicParserTestUtils            (Tall(..),Unto,StrTA(..),to,uns,tShrinks,sListOf,sized1)
import Test.Framework                              (Arbitrary(..),(==>))

fs = flip map funcs $ \(n,(typeValidators,_)) -> (n,(typeValidators, Func $ \_ _ -> lift $ success n))

mk  p ts = let es = uns ts; arg = ArrayT p FU.w2 es in (es,arg)
mkO p ts = let es = uns ts; arg = ArrayO p       es in (es,arg)

{-# ANN mk'   "HLint: ignore Eta reduce" #-}
{-# ANN mkO'  "HLint: ignore Eta reduce" #-}
{-# ANN mkObj "HLint: ignore Eta reduce" #-}
mk'  ts = mk p0 ts
mkO' ts = mkO p0 ts
mkObj  ps = let (ss,tss) = unzip ps; ess = map uns tss in (concat ess,ObjT p0 ws2 $ map (\(x,ys)->PairT (IdT p0 ws2 x) $ ArrayT p0 ws2 ys) $ zip ss ess)
mkObj' ps = let (ss,ts)  = unzip ps; es  = uns ts      in (es,        ObjT p0 ws2 $ map (\(x,y) ->PairT (IdT p0 ws2 x) y)                  $ zip ss es)

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

tableColumnLengthCase w1ps g2as = let moo = map snd w1ps in equalize moo /= moo ==>
  let arrays = map (\(p,es)-> (p,ArrayT p ws2 es)) w1ps; ls = map (length.snd) w1ps; l = head ls; (_,g2) = mkObj g2as in all (not.null.snd) w1ps ==>
  case applyFunc funcs p0 "table" [ArrayT p0 ws2 $ map snd arrays, g2] of
    Left (TableColumnLengthMismatch p expected actual) -> let Just (ArrayT pa _ es) = lookup p arrays in pa == p && l == expected && length es == actual
    e                                                  -> error $ "EngineTestUtils::tableColsCase [Unexpected pattern ["++show e++"]]"

tableHeaderLengthCase pa g1as g2s =
  let g1s = equalize $ map elems $ fst $ mk' g1as
      g1 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1s
      w2 = ObjT   p0 ws2 [PairT (IdT p0 ws2 "col") $ ArrayT pa ws2 g2s]
      (l1,l2) = (length g1s,length g2s)
  in any (not.null) g1s && not (null g2s) && l1 `notElem` [0,l2] ==>
  Left (TableHeaderLengthMismatch pa l1 l2) == applyFunc funcs p0 "table" [g1,w2]

equalize g1ss = map (take l) g1ss where l = minimum $ map length g1ss

mkMultiMeanReturn a1as pa = let a1s = uns a1as; a1rs = map (\(NumT q _ _ x) -> NumO q x) a1s; a1 = ArrayT pa ("","") a1s in (a1,a1rs)

unprecise :: Monad m => m ExpObj -> m ExpObj
unprecise = liftM moo where
  moo (NumO p x) = NumO p $ fromIntegral (floor $ 1000000.0 * x :: Integer) / 1000000.0
  moo e          = error $ "Eval.EngineTestUtils::unprecise [Unexpected pattern ["++show e++"]]"

data TableValidArgs = TableValidArgs [[ExpToken]] [ExpToken] deriving (Show)
instance Arbitrary TableValidArgs where arbitrary = sized1 tall; shrink (TableValidArgs ess es) = mTableValidArgs (shrink $ map (map to) ess)  (tShrinks es)
instance Tall      TableValidArgs where                                                  tall n = mTableValidArgs (sListOf $ sListOf $ tall n)  arbitrary
mTableValidArgs :: Monad m => m [[ExpTS]] -> m [StrTA] -> m TableValidArgs
mTableValidArgs tssa tsa = do tss <- tssa; ts <- tsa; let l = minimum [length tss, length ts] in return $ TableValidArgs (equalize $ map uns $ take l tss) $ uns $ take l ts

mkTableValidArgs pf g1ss g2s useHeader =
  let expectedHeader = concat [map unsafeMarshall g2s                       | useHeader]
      inputHeader    =        [PairT (IdT p0 ws2 "col") $ ArrayT p0 ws2 g2s | useHeader]
      g1 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1ss
      g2 = ObjT p0 ws2 inputHeader
      expected       = Right (TableO pf (map (map unsafeMarshall) g1ss) expectedHeader)
  in (g1,g2,expected)

unsafeMarshallP :: PairToken -> (String,ExpObj)
unsafeMarshallP (PairT (IdT _ _ n) e) = (n,unsafeMarshall e)

unsafeMarshall :: ExpToken -> ExpObj
unsafeMarshall (ArrayT p _ es) = ArrayO p $ map unsafeMarshall es
unsafeMarshall (ObjT   p _ ps) = ObjO   p $ map (\(PairT (IdT _ _ n) e) -> (n,unsafeMarshall e)) ps
unsafeMarshall (StrT p _ v)    = StrO p v
unsafeMarshall (NumT p _ _ v)  = NumO p v
unsafeMarshall (BoolT p _ v)   = BoolO p v
unsafeMarshall (NullT p _)     = NullO p
unsafeMarshall e               = error $ "EngineTestUtils::unsafeMarshall [Unexpected pattern ["++show e++"]]"

keepInRange (NumT p w _ v) l = (NumT p w (show v') v',n) where v' = fromIntegral n; n = floor v `mod` l
keepInRange x _              = error $ "EngineTestUtils::keepInRange [Unexpected pattern ["++show x++"]]"

sortTOn :: Ord a => Int -> [[a]] -> [[a]]
sortTOn n xss = transpose $ map snd $ sort $ zip (xss !! n) $ transpose xss

sortAOn :: Int -> [ExpObj] -> [ExpObj]
sortAOn n arrays = let (ess,mks) = unzip $ map (\(ArrayO p es) -> (es,ArrayO p)) arrays in  zipWith ($) mks $ sortTOn n ess

mkOutOfBoundsTable pn v a2tr cols = let n = floor v; (funs,a2t) = addFunc' "mkTable" a2tr; expected = Left $ IndexOutOfBounds pn n 0 $ length cols - 1 in (n,a2t,funs,expected)
mkOutOfBoundsArray pn v a2as = let n = floor v; (arrays,aOfArrays) = mk' a2as; mArrays = map unsafeMarshall arrays; expected = Left $ IndexOutOfBounds pn n 0 $ length a2as - 1 in (n,aOfArrays,mArrays,expected)

mkSortColArray a1' a2as = let (a1,n) = keepInRange a1' (length a2as); (arrays,aOfArrays) = mk' a2as; mArrays = map unsafeMarshall arrays in (n, a1,aOfArrays,arrays,mArrays)





    