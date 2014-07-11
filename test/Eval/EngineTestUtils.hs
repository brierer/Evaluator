module Eval.EngineTestUtils where

import qualified Eval.FunctionEvalTestUtils1 as FU (w2)

import Control.Applicative                         ((<$>),(<*>))
import Control.Monad                               (liftM)
import Control.Monad.Trans                         (lift)
import Data.Eval                                   (Func(..))
import Data.EvalError                              (EvalError(..))
import Data.ExpObj                                 (ExpObj(..))
import Data.ExpToken                               (PairToken(..),IdToken(..),ExpToken(..))
import Data.List                                   (sort,transpose)
import Eval.Engine                                 (funcs)
import Eval.Function                               (table,num,(<|>),nonEmpty,arrayOf,atom,withFuncs)
import Eval.FunctionEvalTestUtils1                 (ExpOA,TableOA(..),AtomTA,ExpTS(..),p0,ws2,applyFunc)
import Eval.FunctionEvalTestUtils2                 (isTable,isPlot,isArray,isNum,isAtom)
import Parser.MonolithicParserTestUtils            (Unto(..),P(..),NumTA(..),StrTA(..),sListOf,tShrinks)
import Test.Framework                              (Arbitrary(..),(==>))

fs = flip map funcs $ \(n,(typeValidators,_)) -> (n,(typeValidators, Func $ \_ _ -> lift $ success n))

mk  p ts = let es = map un ts; arg = ArrayT p FU.w2 es in (es,arg)
mkO p ts = let es = map un ts; arg = ArrayO p       es in (es,arg)

{-# ANN mk'   "HLint: ignore Eta reduce" #-}
{-# ANN mkO'  "HLint: ignore Eta reduce" #-}
{-# ANN mkObj "HLint: ignore Eta reduce" #-}
mk'  ts = mk p0 ts
mkO' ts = mkO p0 ts
mkObj  ps = let (ss,tss) = unzip ps; ess = map (map un) tss in (concat ess,ObjT p0 ws2 $ map (\(x,ys)->PairT (IdT p0 ws2 x) $ ArrayT p0 ws2 ys) $ zip ss ess)
mkObj' ps = let (ss,ts)  = unzip ps; es  = map un ts      in (es,        ObjT p0 ws2 $ map (\(x,y) ->PairT (IdT p0 ws2 x) y)                  $ zip ss es)

oneArrayOf g1ras w1as = let es = map un g1ras; g1 = ArrayT p0 ws2 es; (w1s,w1) = mk' w1as in (fs,g1,w1s,w1)

addFunc  fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):fs;    g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)
addFunc' fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):funcs; g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)

success n = return $ StrO p0 $ "Mocked Success ["++n++"]"

toArray (x,y) e = ArrayT p0 FU.w2 $ replicate ((x+y) `mod` 100) e

tablesAndPlots :: [ExpOA] -> [ExpOA]
tablesAndPlots xs = let ys = map un xs :: [ExpObj] in  map to (filter ((||) <$> isTable <*> isPlot) ys)

typeMismatchSortColCase name (P p) (NumTA _ g1) (TableValidArgs g2ss _) (TableOA g2r) (ExpTS w1) w2ass w2'as (ExpTS w2'') =
  let g2 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g2ss; w2ss = map (map un) w2ass 
      w2 = ArrayT p0 ws2 $ map (ArrayT p0 ws2) w2ss; (w2's,w2') = mk' w2'as; (xs,g2') = addFunc "table" g2r in 
  not (isNum w1) && any (not.null) g2ss && any (not.isAtom) (concat w2ss) && any (not.isArray) w2's && not (isArray w2'') ==>
    withFuncs xs  num                 w1   == applyFunc xs p name [w1,g2]   &&
    withFuncs xs  num                 w1   == applyFunc xs p name [w1,g2']  &&
    withFuncs xs  num                 w1   == applyFunc xs p name [w1,w2]   &&
    withFuncs xs  num                 w1   == applyFunc xs p name [w1,w2''] &&
    withFuncs xs (table <|> tableArg) w2   == applyFunc xs p name [g1,w2]   &&
    withFuncs xs (table <|> tableArg) w2'  == applyFunc xs p name [g1,w2']  &&
    withFuncs xs (table <|> tableArg) w2'' == applyFunc xs p name [g1,w2''] &&
    success name                           == applyFunc xs p name [g1,g2]   &&
    success name                           == applyFunc xs p name [g1,g2']
    
tableArg = nonEmpty $ arrayOf $ nonEmpty $ arrayOf atom

emptyArray (ArrayT _ _ es) = null es
emptyArray e               = error $ "Eval.EngineTestUtils::emptyArray [Unexpected pattern ["++show e++"]]"

emptySortColCase name pa pt n (TableValidArgs g2ss _) =
  let g2  = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g2ss
      w2' = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g2ss ++ [w2]
      (_,w2) = mk pa ([] :: [ExpTS]) in any (not.null) g2ss ==>
    Left (IllegalEmpty pa) == applyFunc fs pt name [n, w2 ] &&
    Left (IllegalEmpty pa) == applyFunc fs pt name [n, w2'] &&
    success name          == applyFunc fs pt name [n, g2 ]

tableColumnLengthCase w1ps g2as = let ess = map snd w1ps in equalize ess /= ess ==>
  let arrays = map (\(p,es)-> (p,ArrayT p ws2 es)) w1ps; ls = map (length.snd) w1ps; l = head ls; (_,g2) = mkObj g2as in all (not.null.snd) w1ps ==>
  case applyFunc funcs p0 "table" [ArrayT p0 ws2 $ map snd arrays, g2] of
    Left (TableColumnLengthMismatch p expected actual) -> let Just (ArrayT pa _ es) = lookup p arrays in pa == p && l == expected && length es == actual
    e                                                  -> error $ "EngineTestUtils::tableColsCase [Unexpected pattern ["++show e++"]]"

tableHeaderLengthCase pa (TableValidArgs g1ss _) (TableValidArgs _ g2s) = 
  let g1  = ArrayT p0 ws2 $ map (ArrayT p0 ws2) g1ss
      w2 = ObjT   p0 ws2 [PairT (IdT p0 ws2 "col") $ ArrayT pa ws2 g2s]
      (l1,l2) = (length g1ss,length g2s)
  in any (not.null) g1ss && not (null g2s) && l1 /= l2 ==> 
     Left (TableHeaderLengthMismatch pa l1 l2) == applyFunc funcs p0 "table" [g1,w2]

equalize g1ss = map (take l) g1ss where l = minimum $ map length g1ss

mkMultiMeanReturn a1as pa = let a1s = map un a1as; a1rs = map (\(NumT q _ _ x) -> NumO q x) $ filter isNum a1s; a1 = ArrayT pa ("","") a1s in (a1,a1rs)

unprecise :: Monad m => m ExpObj -> m ExpObj
unprecise = liftM moo where
  moo (NumO p x) = NumO p $ fromIntegral (floor $ 1000000.0 * x :: Integer) / 1000000.0
  moo e          = error $ "Eval.EngineTestUtils::unprecise [Unexpected pattern ["++show e++"]]"

data TableValidArgs = TableValidArgs [[ExpToken]] [ExpToken] deriving (Show)
instance Arbitrary TableValidArgs where 
  arbitrary                      = mTableValidArgs (sListOf $ sListOf arbitrary)  arbitrary
  shrink (TableValidArgs ess es) = mTableValidArgs (shrink $ map (map to) ess)   (tShrinks es)
mTableValidArgs tssa tsa = do tss <- tssa; ts <- tsa; let l = minimum [length tss, length ts] in return $ TableValidArgs (equalize $ map (map un) $ take l tss) $ map un $ take l ts
mTableValidArgs :: Monad m => m [[AtomTA]] -> m [StrTA] -> m TableValidArgs

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
mkOutOfBoundsArray pn v a2ss = let n = floor v; (_,aOfArrays,mArrays) = mkArrays a2ss; expected = Left $ IndexOutOfBounds pn n 0 $ length a2ss - 1 in (n,aOfArrays,mArrays,expected)

mkSortColArray a1' a2ss = let (a1,n) = keepInRange a1' (length a2ss); (arrays,aOfArrays,mArrays) = mkArrays a2ss in (n, a1,aOfArrays,arrays,mArrays)

mkArrays a2ss = let arrays = map (ArrayT p0 ws2) a2ss; aOfArrays = ArrayT p0 ws2 arrays; mArrays = map unsafeMarshall arrays in (arrays,aOfArrays,mArrays)



    