{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Eval.EvalTestUtils where

import qualified Data.Map as M                        (fromList)
import qualified Data.Set as S                        (empty,toList,insert)

import Data.Function                                  (on)
import Data.List                                      (nubBy,sort,nub,inits)
import Data.Maybe                                     (fromMaybe)
import Data.Token                                     (ProgToken(..),FormToken(..),PairToken(..),ExpToken(..))
import Control.Applicative                            ((<$>),(<*>))
import Control.Monad                                  (liftM,zipWithM)
import Eval.MultiPass                                 (Table,initTable,formVal,pairVal,mapPair,mapMPair)
import Parser.ParserTestUtils                         (ProgTA(..),sList)
import Test.Framework                                 (Arbitrary,arbitrary,shrink,elements)
import Text.ParserCombinators.Parsec.Error            (ParseError,errorMessages)

instance Eq ParseError where (==) a b = errorMessages a == errorMessages b

class HasProg a where
  forms :: a -> [FormToken]
  fromForms :: [FormToken] -> a
  toToken :: a -> ProgToken
  toToken = fromForms.forms

instance HasProg ProgToken where
  forms (ProgT fs) = fs
  fromForms = ProgT

instance HasProg ProgTA where
  forms (ProgTA p) = forms p
  fromForms = ProgTA .fromForms
        
data UniqueDefs = UniqueDefs ProgTA deriving (Eq,Show)
instance HasProg UniqueDefs where
  forms (UniqueDefs prog) = forms prog
  fromForms = UniqueDefs .fromForms
instance Arbitrary UniqueDefs where
  arbitrary                = mUniqueDefs arbitrary
  shrink (UniqueDefs prog) = mUniqueDefs (shrink prog)
mUniqueDefs = liftM (fromForms.uniqueForms.forms)
toUniqueDefs = UniqueDefs
unUniqueDefs (UniqueDefs p) = p
uniqueForms = nubBy ((==) `on` formName)

data MultiDefs  = MultiDefs UniqueDefs String deriving (Eq,Show)
instance Arbitrary MultiDefs where
  arbitrary                 = mMultiDefs arbitrary
  shrink (MultiDefs prog _) = mMultiDefs (shrink prog)
mMultiDefs = liftM (uncurry (MultiDefs . fromForms) . mkMultiDefs . forms)
mkMultiDefs xs = let fs = xs++xs in (fs,firstName fs)
  where firstName = fromMaybe "" . firstDup . map formName
        firstDup (y:ys) | y `elem` ys = Just y | otherwise = firstDup ys
        firstDup [] = Nothing

data ValidVars = ValidVars UniqueDefs deriving (Eq,Show)
instance HasProg ValidVars where
  forms (ValidVars prog) = forms prog
  fromForms = ValidVars .fromForms
instance Arbitrary ValidVars where
  arbitrary               = mValidProg arbitrary     elements
  shrink (ValidVars prog) = mValidProg (shrink prog) id
mValidProg pa f = liftM fromForms (pa >>= replaceAllVars f . forms)

replaceAllVars _ []  = return []
replaceAllVars f (FormT n v:fs) = do
  v' <- replaceVars (map formName fs) f v
  rest <- replaceAllVars f fs
  return $ FormT n v': rest

replaceVars ns f (FuncT w n es) = liftM (FuncT w n) $ mapM (replaceVars ns f) es
replaceVars ns f (ArrayT w es)  = liftM (ArrayT w)  $ mapM (replaceVars ns f) es
replaceVars ns f (ObjT w ps)    = liftM (ObjT w)    $ mapM (mapMPair $ replaceVars ns f) ps
replaceVars [] _ (VarT w _)     = return (NullT w)
replaceVars ns f (VarT w _)     = liftM (VarT w) $ f ns
replaceVars _  _ e              = return e

data UndefVars = UndefVars ValidVars String deriving (Show)
instance Arbitrary UndefVars where
  arbitrary              = mUndefProg arbitrary  elements
  shrink (UndefVars p _) = mUndefProg (shrink p) id
mUndefProg pa f = let empty = UndefVars (fromForms []) "" in do
  fs <- liftM forms pa
  let ns = getRefed fs
  nullGuard ns empty $ do
    toRemove <- f ns
    let fs' = filter ((/=toRemove).formName) fs
    return $ UndefVars (fromForms fs') toRemove
getRefed = S.toList . f S.empty
  where f acc []             = acc
        f acc (FormT _ e:fs) = f (g acc e) fs
        g acc (FuncT _ _ es) = foldl g acc es
        g acc (ArrayT _ es)  = foldl g acc es
        g acc (ObjT _ ps)    = foldl g acc $ map pairVal ps
        g acc (VarT _ n)     = S.insert n acc
        g acc _              = acc

data CycleVars = CycleVars ValidVars [String] deriving (Show)
instance Arbitrary CycleVars where
  arbitrary              = mCycleProg arbitrary  elements
  shrink (CycleVars p _) = mCycleProg (shrink p) id
mCycleProg pa f = let empty = CycleVars (fromForms []) [] in do
  fs <- liftM (filter hasVarF.forms) pa
  nullGuard fs empty $ do
    let ns = map formName fs
    fs' <- zipWithM (makeCycle f) fs $ tail $ cycle ns
    return $ CycleVars (fromForms fs') (sort ns)
makeCycle f (FormT n e) m = liftM (FormT n) $ replaceVars [m] f e

data ValidFuncs = ValidFuncs UniqueDefs [String] deriving (Eq,Show)
instance HasProg ValidFuncs where
  forms (ValidFuncs prog _) = forms prog
  fromForms = ValidFuncs <$> fromForms <*> funcNamesF
instance Arbitrary ValidFuncs where
  arbitrary                    =         mValidFuncs arbitrary
  shrink p@(ValidFuncs prog _) = diff p$ mValidFuncs (shrink prog)
mValidFuncs = liftM ((ValidFuncs <$> fromForms <*> funcNamesF).insertTopShow.replaceNonTopShows.removeTopShow.forms)

funcNamesF :: [FormToken] -> [String]
funcNamesF = filter (/="show").nub.funcNames.map formVal
funcNames :: [ExpToken] -> [String]
funcNames = concatMap f where
  f (FuncT _ n es) = n:funcNames es
  f (ArrayT _ es)  = funcNames es
  f (ObjT _ ps)    = funcNames $ map pairVal ps
  f _              = []

insertTopShow fs = FormT "show" (FuncT "" "show" $ map formVal fs):fs

replaceNonTopShows = map f where 
  f (FormT n e) = FormT n $ g e
  g (FuncT w n es) = FuncT w (case n of "show" -> "notShow"; _ -> n) $ map g es
  g (ArrayT w es)  = ArrayT w $ map g es
  g (ObjT w ps)    = ObjT w $ map (mapPair g) ps
  g e              = e  
  
removeTopShow []                                   = []
removeTopShow (FormT "show" (FuncT _ "show" _):fs) = fs
removeTopShow (f:fs)                               = f:removeTopShow fs

data UndefFuncs = UndefFuncs ValidFuncs String String deriving (Eq,Show)
instance Arbitrary UndefFuncs where
  arbitrary                      =          mUndefFuncs arbitrary                         elements    elements
  shrink p@(UndefFuncs prog _ _) = diff p $ mUndefFuncs (shrink $ removeUnderscores prog) tail        tail
mUndefFuncs pa f1 f2 = let empty = UndefFuncs (fromForms []) "" "" in do
  ValidFuncs prog fns <- pa
  nullGuard fns empty $ do
    fn <- f1 fns
    let fs = forms prog
    fn' <- liftM (++"_") $ f1 fns
    f <- liftM (replaceFunc fn fn') $ f2 $ filter (usesFunc fn) fs
    let fs' = map (\f' -> if formName f == formName f' then f else f') fs
    return $ UndefFuncs (ValidFuncs (fromForms fs') fns) (formName f) fn'

removeUnderscores = fromForms.map f.forms where
  f (FormT n e) = FormT n $ g e
  g (FuncT w n es) = FuncT w (filter (/='_') n) $ map g es
  g (ArrayT w es)  = ArrayT w $ map g es
  g (ObjT w ps)    = ObjT w $ map (mapPair g) ps
  g e              = e

replaceFunc :: String -> String -> FormToken -> FormToken
replaceFunc fn n' (FormT n e) = FormT n $ replaceFuncE fn n' e

replaceFuncE :: String -> String -> ExpToken -> ExpToken
replaceFuncE fn n' (FuncT w n es) = FuncT w (if fn == n then n' else n) $ map (replaceFuncE fn n') es
replaceFuncE fn n' (ArrayT w es)  = ArrayT w $ map (replaceFuncE fn n') es
replaceFuncE fn n' (ObjT w ps)    = ObjT w   $ map (mapPair $ replaceFuncE fn n') ps
replaceFuncE _  _  e              = e

usesFunc:: String -> FormToken -> Bool
usesFunc fn (FormT _ e) = usesFuncE fn e

usesFuncE :: String -> ExpToken -> Bool
usesFuncE fn (FuncT _ n es) = fn == n || any (usesFuncE fn) es
usesFuncE fn (ArrayT _ es)  =            any (usesFuncE fn) es
usesFuncE fn (ObjT _ ps)    =            any (usesFuncE fn.pairVal) ps
usesFuncE _  _              = False

data NonTopShowFuncs = NonTopShowFuncs ValidFuncs String deriving (Eq,Show)
instance Arbitrary NonTopShowFuncs where

data NoShowFuncs = NoShowFuncs ValidFuncs deriving (Eq,Show)
instance HasProg NoShowFuncs where
  forms (NoShowFuncs prog) = forms prog
  fromForms = NoShowFuncs .fromForms
instance Arbitrary NoShowFuncs where
  arbitrary                 = mNoShowFuncs arbitrary
  shrink (NoShowFuncs prog) = mNoShowFuncs (shrink prog)
mNoShowFuncs = liftM (fromForms.removeTopShow.forms)

{- | Utils -}
fromProgForms = M.fromList.map toTuple.forms
derefValidProg = fromForms.derefAll.forms
derefValidProg' = initTable'.derefValidProg
initTable' = (\(Right x)->x).initTable.toToken

derefAll []             = []
derefAll (FormT n v:fs) = let moo1 = FormT n (derefOne fs v):derefAll fs
                          in  if any hasVarF moo1 then derefAll moo1 else moo1

hasVarF (FormT _ e) = hasVar e
hasVarP (PairT _ e) = hasVar e
hasVar (FuncT _ _ es) = any hasVar es
hasVar (ArrayT _ es)  = any hasVar es
hasVar (ObjT _ ps)    = any hasVarP ps
hasVar (VarT _ _)     = True
hasVar _              = False

derefOne fs (FuncT w m es) = FuncT w m  $ map (derefOne fs) es
derefOne fs (ArrayT w es)  = ArrayT w   $ map (derefOne fs) es
derefOne fs (ObjT w ps)    = ObjT w     $ map (derefP fs) ps
derefOne fs (VarT _ m)     = fromMaybe (error $ "Couldn't find var ["++m++"] in prog ["++show fs++"]") $ lookup m $ map toTuple fs
derefOne _  e              = e
derefP   fs (PairT n e)    = PairT n $ derefOne fs e

nullGuard xs ifNull action = if null xs then return ifNull else action

toTuple (FormT a b) = (a,b)
formName = fst.toTuple

nonEmpty = not.null.forms

diff x = filter (/= x)

{-| Monomorphism restriction -}
mUniqueDefs :: Monad m => m ProgTA     -> m UniqueDefs
mMultiDefs  :: Monad m => m UniqueDefs -> m MultiDefs
mValidProg  :: Monad m => m UniqueDefs -> ([String] -> m String) -> m ValidVars
mUndefProg  :: Monad m => m ValidVars  -> ([String] -> m String) -> m UndefVars
mCycleProg  :: Monad m => m ValidVars  -> ([String] -> m String) -> m CycleVars

mValidFuncs  :: Monad m => m UniqueDefs -> m ValidFuncs
mUndefFuncs  :: Monad m => m ValidFuncs -> ([String] -> m String) -> ([FormToken] -> m FormToken) -> m UndefFuncs
mNoShowFuncs :: Monad m => m ValidFuncs -> m NoShowFuncs 

derefValidProg  :: HasProg a => a -> a
derefValidProg' :: HasProg a => a -> Table
fromProgForms   :: HasProg a => a -> Table
initTable'      :: HasProg a => a -> Table
nonEmpty        :: HasProg a => a -> Bool




















