module Eval.Engine.EngineTestUtils where

import Control.Applicative hiding ((<|>))
import Prelude             hiding (any,null)

import qualified Prelude as P

import Control.Monad                               
import Control.Monad.Trans                         
import Data.Eval                                   
import Data.EvalError                              
import Data.ExpObj                                 
import Data.ExpToken                               
import Data.List                                   
import Eval.Engine                                 
import Eval.Function                               
import Eval.Function.FunctionEvalTestUtils1        
import Eval.Function.FunctionEvalTestUtils2        
import Parser.MonolithicParserTestUtils            
import Test.Framework                              

funcMocks = flip map funcs $ \(n,(typeValidators,_)) -> (n,(typeValidators, Func $ \_ _ -> lift $ success n))

mk  p ts = let es = map un ts; arg = mkArr' p es in (es,arg)
mkO p ts = let es = map un ts; arg = ArrayO p es in (es,arg)

{-# ANN mk'       "HLint: ignore Eta reduce" #-}
{-# ANN mkO'      "HLint: ignore Eta reduce" #-}
{-# ANN mkObjFrom "HLint: ignore Eta reduce" #-}
mk'  ts = mk p0 ts
mkO' ts = mkO p0 ts
mkObjFrom  ps = let (ss,tss) = unzip ps; ess = map (map un) tss in (concat ess,mkObj $ zipWith (\x ys->mkPair x $ mkArr ys) ss ess)
mkObjFrom' ps = let (ss,ts)  = unzip ps; es  = map un ts        in (es,        mkObj $ zipWith mkPair                       ss es)

oneArrayOf g1ras w1as = let es = map un g1ras; g1 = mkArr es; (w1s,r) = mk' w1as in (funcMocks,g1,w1s,r)

addFunc  fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):funcMocks; g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)
addFunc' fn r = let xs' = (fn,([],Func $ \_ _ -> return r)):funcs;     g = FuncT "" (IdT p0 ("","") fn) [] in (xs',g)

success n = return $ StrO p0 $ "Mocked Success ["++n++"]"

toArray (x,y) e = mkArr $ replicate ((x+y) `mod` 100) e

tablesAndPlots :: [ExpOA] -> [ExpOA]
tablesAndPlots xs = let ys = map un xs :: [ExpObj] in  map to (filter ((||) <$> isTable <*> isPlot) ys)

typeMismatchSortColCase name (P p) (NumTA _ g1) (TableValidArgs g2ss _) (TableOA g2r) (ExpTS w1) w2ass w2'as (ExpTS w2'') =
  let g2 = mkArr $ map mkArr g2ss; w2ss = map (map un) w2ass 
      w2 = mkArr $ map mkArr w2ss; (w2's,w2') = mk' w2'as; (xs,g2') = addFunc "table" g2r in 
  not (isNum w1) && P.any (not.P.null) g2ss && P.any (not.isAtom) (concat w2ss) && P.any (not.isArray) w2's && not (isArray w2'') ==>
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

emptyArray (ArrT _ _ es) = P.null es
emptyArray e             = error $ "Eval.EngineTestUtils::emptyArray [Unexpected pattern ["++show e++"]]"

emptySortColCase name pa pt n (TableValidArgs g2ss _) =
  let g2  = mkArr $ map mkArr g2ss
      w2' = mkArr $ map mkArr g2ss ++ [w2]
      (_,w2) = mk pa ([] :: [ExpTS]) in P.any (not.P.null) g2ss ==>
    Left (IllegalEmpty pa) == applyFunc funcMocks pt name [n, w2 ] &&
    Left (IllegalEmpty pa) == applyFunc funcMocks pt name [n, w2'] &&
    success name           == applyFunc funcMocks pt name [n, g2 ]

tableColumnLengthCase w1ps g2as = let ess = map snd w1ps in equalize ess /= ess ==>
  let arrays = map (\(x,y)->(x,mkArr' x y)) w1ps; ls = map (length.snd) w1ps; (_,g2) = mkObjFrom g2as in all (not.P.null.snd) w1ps ==>
  case applyFunc funcs p0 "table" [mkArr $ map snd arrays, g2] of
    Left (TableColumnLengthMismatch p expected actual) -> let Just (ArrT pa _ es) = lookup p arrays in pa == p && head ls == expected && length es == actual
    e                                                  -> error $ "EngineTestUtils::tableColsCase [Unexpected pattern ["++show e++"]]"

tableHeaderLengthCase pa (TableValidArgs g1ss _) (TableValidArgs _ g2s) = 
  let g1 = mkArr  $ map mkArr g1ss
      w2 = mkObj  [mkPair "col" $ mkArr' pa g2s]
      (l1,l2) = (length g1ss,length g2s)
  in P.any (not.P.null) g1ss && not (P.null g2s) && l1 /= l2 ==> 
     Left (TableHeaderLengthMismatch pa l1 l2) == applyFunc funcs p0 "table" [g1,w2]

mkMultiMeanReturn a1as pa = let a1s = map un a1as; a1rs = map (\(NumT q _ _ x) -> NumO q x) $ filter isNum a1s; a1 = mkArr' pa a1s in (a1,a1rs)

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
      inputHeader    =        [mkPair "col" $ mkArr g2s | useHeader]
      g1 = mkArr $ map mkArr g1ss
      g2 = mkObj inputHeader
      expected       = Right (TableO pf (map (map unsafeMarshall) g1ss) expectedHeader)
  in (g1,g2,expected)

unsafeMarshallP :: PairToken -> (String,ExpObj)
unsafeMarshallP (PairT (IdT _ _ n) e) = (n,unsafeMarshall e)

unsafeMarshall :: ExpToken -> ExpObj
unsafeMarshall (ArrT p _ es)   = ArrayO p $ map unsafeMarshall es
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

mkArrays a2ss = let arrays = map mkArr a2ss; aOfArrays = mkArr arrays; mArrays = map unsafeMarshall arrays in (arrays,aOfArrays,mArrays)



    