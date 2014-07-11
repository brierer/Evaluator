{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Eval.MultiPass.MultiPassEvalTestUtils where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.ExpToken
import Data.Function
import Data.List
import Data.Maybe
import Eval.MultiPass
import Parser.Monolithic
import Parser.MonolithicParserTestUtils
import Test.Framework

class HasProg a where
  forms     :: a -> [FormToken]
  fromForms :: [FormToken] -> a

instance HasProg ProgToken where forms (ProgT _ fs) = fs;      fromForms = ProgT p0
instance HasProg ProgTA    where forms (ProgTA p)   = forms p; fromForms = ProgTA .fromForms

data UniqueDefs = UniqueDefs ProgTA deriving (Eq,Show)
instance HasProg UniqueDefs where forms (UniqueDefs prog) = forms prog; fromForms = UniqueDefs .fromForms
instance Arbitrary UniqueDefs where
  arbitrary                = mUniqueDefs arbitrary
  shrink (UniqueDefs prog) = mUniqueDefs (sShrink prog)
mUniqueDefs = liftM (fromForms.uniqueForms.forms)
uniqueForms = nubBy ((==) `on` formName)

data MultiDefs  = MultiDefs UniqueDefs Pos String deriving (Eq,Show)
instance Arbitrary MultiDefs where
  arbitrary                   = mMultiDefs arbitrary
  shrink (MultiDefs prog _ _) = mMultiDefs (sShrink prog)
mMultiDefs pa = let emptyProg = MultiDefs (fromForms []) p0 "" in do
  fs <- liftM forms pa
  nullGuard fs emptyProg $ do
    let (FormT (IdT p _ n) _) = head fs
        fs' = mkForm p0 n (mkNull p0):tail fs
    return $ MultiDefs (fromForms $ fs'++fs++fs') p n

data ValidVars = ValidVars UniqueDefs deriving (Eq,Show)
instance HasProg ValidVars where
  forms (ValidVars prog) = forms prog
  fromForms = ValidVars .fromForms
instance Arbitrary ValidVars where
  arbitrary               = mValidVars arbitrary      elements
  shrink (ValidVars prog) = mValidVars (sShrink prog) id
mValidVars pa f = liftM fromForms (pa >>= replaceAllVars f . forms)

replaceAllVars _ []  = return []
replaceAllVars f (FormT n v:fs) = do
  v' <- replaceVars (map formName fs) f v
  rest <- replaceAllVars f fs
  return $ FormT n v': rest

replaceVars ns f (FuncT w n es)     = liftM (FuncT w n) $ mapM (replaceVars ns f) es
replaceVars ns f (ArrT p w es)      = liftM (ArrT p w)  $ mapM (replaceVars ns f) es
replaceVars ns f (ObjT p w ps)      = liftM (ObjT p w)  $ mapM (mapMPair $ replaceVars ns f) ps
replaceVars [] _ (VarT (IdT _ w _)) = return (NullT p0 w)
replaceVars ns f (VarT (IdT q w _)) = liftM (VarT .IdT q w) $ f ns
replaceVars _  _ e                  = return e

data UndefVars = UndefVars ValidVars Pos String deriving (Show)
instance Arbitrary UndefVars where
  arbitrary                   = mUndefVars arbitrary                           elements elements
  shrink (UndefVars prog _ _) = mUndefVars (sShrink $ removeUnderscoresV prog) id       id
mUndefVars pa f1 f2 = let emptyProg = UndefVars (fromForms []) p0 "" in do
  fs <- liftM forms pa
  let ns = getRefed fs
  nullGuard ns emptyProg $ do
    n <- f1 ns
    let fs' = filter (usesVar n) fs
    nullGuard fs' emptyProg $ do
      moo <- f2 fs'
      let n' = n ++ "_"
          (f,p) = flip runState p0 $ replaceOneVar n n' moo
          result = map (\f' -> if formName f == formName f' then f else f') fs
      return $ UndefVars (fromForms result) p n'

removeUnderscoresV = fromForms.map f.forms where
  f (FormT n e)         = FormT n $ g e
  g (FuncT w i es)      = FuncT w i $ map g es
  g (ArrT p w es)       = ArrT p w  $ map g es
  g (ObjT p w ps)       = ObjT p w $ map (mapPair g) ps
  g (VarT (IdT q w' n)) = VarT $ IdT q w' (filter (/='_') n)
  g e                   = e

replaceOneVar :: String -> String -> FormToken -> State Pos FormToken
replaceOneVar fn n' (FormT n e) = liftM (FormT n) $ replaceOneVarE fn n' e

replaceOneVarE :: String -> String -> ExpToken -> State Pos ExpToken
replaceOneVarE fn n' (FuncT w i es)     = liftM (FuncT w i) $ mapM (replaceOneVarE fn n') es
replaceOneVarE fn n' (ArrT p w es)      = liftM (ArrT p w)  $ mapM (replaceOneVarE fn n') es
replaceOneVarE fn n' (ObjT p w ps)      = liftM (ObjT p w)  $ mapM (mapMPair $ replaceOneVarE fn n') ps
replaceOneVarE fn n' (VarT (IdT q w n)) = liftM (VarT . IdT q w) $ getNewName fn n n' q
replaceOneVarE _  _  e                  = return e

getNewName fn n n' q = do
  oldP <- get
  let (newN,newP) = case (fn == n,oldP == p0) of (True,True) -> (n',q); (True,_) -> (n',oldP); _ -> (n,oldP)
  put newP
  return newN

usesVar :: String -> FormToken -> Bool
usesVar fn (FormT _ e) = usesVarE fn e

usesVarE :: String -> ExpToken -> Bool
usesVarE fn (FuncT _ _ es)     = any (usesFuncE fn) es
usesVarE fn (ArrT _ _ es)      = any (usesFuncE fn) es
usesVarE fn (ObjT _ _ ps)      = any (usesFuncE fn.pairVal) ps
usesVarE fn (VarT (IdT _ _ n)) = fn == n
usesVarE _  _                  = False

getRefed = S.toList . f S.empty
  where f acc []                 = acc
        f acc (FormT _ e:fs)     = f (g acc e) fs
        g acc (FuncT _ _ es)     = foldl g acc es
        g acc (ArrT _ _ es)      = foldl g acc es
        g acc (ObjT _ _ ps)      = foldl g acc $ map pairVal ps
        g acc (VarT (IdT _ _ n)) = S.insert n acc
        g acc _                  = acc

data CycleVars = CycleVars ValidVars [(Pos,String)] deriving (Show)
instance Arbitrary CycleVars where
  arbitrary                 = mCycleVars arbitrary      elements
  shrink (CycleVars prog _) = mCycleVars (sShrink prog) id
mCycleVars pa f = let emptyProg = CycleVars (fromForms []) [] in do
  fs <- liftM (filter hasVarF.forms) pa
  nullGuard fs emptyProg $ do
    let ns = map formName fs
    fs' <- zipWithM (makeCycle f) fs $ tail $ cycle ns
    return $ CycleVars (fromForms fs') (flipZip $ sort $ zip ns $ map formPos fs) where flipZip = uncurry (flip zip).unzip
makeCycle f (FormT n e) m = liftM (FormT n) $ replaceVars [m] f e

data ValidFuncs = ValidFuncs ValidVars [String] deriving (Eq,Show)
instance HasProg ValidFuncs where
  forms (ValidFuncs prog _) = forms prog
  fromForms = ValidFuncs <$> fromForms <*> funcNamesF
instance Arbitrary ValidFuncs where
  arbitrary                    =         mValidFuncs arbitrary
  shrink p@(ValidFuncs prog _) = diff p$ mValidFuncs (sShrink prog)
mValidFuncs = liftM ((ValidFuncs <$> fromForms <*> funcNamesF).insertTopShow.replaceNonTopShows.removeTopShow.forms)

funcNamesF :: [FormToken] -> [String]
funcNamesF = filter (/="show").nub.funcNames.map formVal
funcNames :: [ExpToken] -> [String]
funcNames = concatMap f where
  f (FuncT _ (IdT _ _ n) es) = n:funcNames es
  f (ArrT _ _ es)            = funcNames es
  f (ObjT _ _ ps)            = funcNames $ map pairVal ps
  f _                        = []

insertTopShow fs = mkForm p0 "show" (mkFunc p0 "show" $ map formVal fs):fs

replaceNonTopShows = map f where
  f (FormT n e)               = FormT n $ g e
  g (FuncT w (IdT q w' n) es) = FuncT w (IdT q w' (case n of "show" -> "notShow"; _ -> n)) $ map g es
  g (ArrT p w es)             = ArrT p w $ map g es
  g (ObjT p w ps)             = ObjT p w $ map (mapPair g) ps
  g e                         = e

removeTopShow []                                                       = []
removeTopShow (FormT (IdT _ _ "show") (FuncT _ (IdT _ _ "show") _):fs) = fs
removeTopShow (f:fs)                                                   = f:removeTopShow fs

data UndefFuncs = UndefFuncs ValidFuncs Pos String deriving (Eq,Show)
instance Arbitrary UndefFuncs where
  arbitrary                      =          mUndefFuncs arbitrary                          elements    elements
  shrink p@(UndefFuncs prog _ _) = diff p $ mUndefFuncs (sShrink $ removeUnderscores prog) tail        tail
mUndefFuncs pa f1 f2 = let emptyProg = UndefFuncs (fromForms []) p0 "" in do
  ValidFuncs prog fns <- pa
  nullGuard fns emptyProg $ do
    fn <- f1 fns
    let fs = forms prog
        fn' = fn ++ "_"
    moo <- f2 $ filter (usesFunc fn) fs
    let (f,p) = flip runState p0 $ replaceOneFunc fn fn' moo
        fs' = map (\f' -> if formName f == formName f' then f else f') fs
    return $ UndefFuncs (ValidFuncs (fromForms fs') fns) p fn'

removeUnderscores = fromForms.map f.forms where
  f (FormT n e)               = FormT n $ g e
  g (FuncT w (IdT q w' n) es) = FuncT w (IdT q w' (filter (/='_') n)) $ map g es
  g (ArrT p w es)             = ArrT p w $ map g es
  g (ObjT p w ps)             = ObjT p w $ map (mapPair g) ps
  g e                         = e

replaceOneFunc :: String -> String -> FormToken -> State Pos FormToken
replaceOneFunc fn n' (FormT n e) = liftM (FormT n) $ replaceOneFuncE fn n' e

replaceOneFuncE :: String -> String -> ExpToken -> State Pos ExpToken
replaceOneFuncE fn n' (FuncT w (IdT q w' n) es) = getNewName fn n n' q >>= \newN -> liftM (FuncT w (IdT q w' newN)) $ mapM (replaceOneFuncE fn n') es
replaceOneFuncE fn n' (ArrT p w es)             = liftM (ArrT p w) $ mapM (replaceOneFuncE fn n') es
replaceOneFuncE fn n' (ObjT p w ps)             = liftM (ObjT p w)   $ mapM (mapMPair $ replaceOneFuncE fn n') ps
replaceOneFuncE _  _  e                         = return e

usesFunc :: String -> FormToken -> Bool
usesFunc fn (FormT _ e) = usesFuncE fn e

usesFuncE :: String -> ExpToken -> Bool
usesFuncE fn (FuncT _ (IdT _ _ n) es) = fn == n || any (usesFuncE fn) es
usesFuncE fn (ArrT _ _ es)            =            any (usesFuncE fn) es
usesFuncE fn (ObjT _ _ ps)            =            any (usesFuncE fn.pairVal) ps
usesFuncE _  _                        = False

data NonTopShowFuncs = NonTopShowFuncs ValidFuncs Pos deriving (Eq,Show)
instance Arbitrary NonTopShowFuncs where
  arbitrary                         =          mNonTopShowFuncs arbitrary                    elements    elements
  shrink p@(NonTopShowFuncs prog _) = diff p $ mNonTopShowFuncs (sShrink $ removeShows prog) tail        tail
mNonTopShowFuncs pa f1 f2 = let emptyProg = NonTopShowFuncs (fromForms []) p0 in do
  ValidFuncs prog fns <- pa
  nullGuard fns emptyProg $ do
    fn <- f1 fns
    let fs = forms prog
    moo <- f2 $ filter (usesFuncNonTop fn) fs
    let (f,p) = flip runState p0 $ replaceOneNonTopFunc fn "show" moo
        fs' = map (\f' -> if formName f == formName f' then f else f') fs
    return $ NonTopShowFuncs (ValidFuncs (fromForms fs') fns) p

removeShows = fromForms.map f.forms where
  f (FormT n (FuncT w m es))  = FormT n $ FuncT w m $ map g es
  f (FormT n e)               = FormT n $ g e
  g (FuncT w (IdT q w' n) es) = FuncT w (IdT q w' (if n == "show" then "notShow" else n)) $ map g es
  g (ArrT p w es)             = ArrT p w $ map g es
  g (ObjT p w ps)             = ObjT p w $ map (mapPair g) ps
  g e                         = e

replaceOneNonTopFunc :: String -> String -> FormToken -> State Pos FormToken
replaceOneNonTopFunc fn n' (FormT n (FuncT w m es)) = liftM (FormT n .FuncT w m) $ mapM (replaceOneFuncE fn n') es
replaceOneNonTopFunc fn n' (FormT n e)              = liftM (FormT n) $ replaceOneFuncE fn n' e

usesFuncNonTop :: String -> FormToken -> Bool
usesFuncNonTop fn (FormT _ (FuncT _ _ es)) = any (usesFuncE fn) es
usesFuncNonTop fn (FormT _ e)              = usesFuncE fn e

data NoShowFuncs = NoShowFuncs ValidFuncs deriving (Eq,Show)
instance HasProg NoShowFuncs where
  forms (NoShowFuncs prog) = forms prog
  fromForms = NoShowFuncs .fromForms
instance Arbitrary NoShowFuncs where
  arbitrary                 = mNoShowFuncs arbitrary
  shrink (NoShowFuncs prog) = mNoShowFuncs (sShrink prog)
mNoShowFuncs = liftM (fromForms.removeTopShow.forms)

{- | Utils -}
unsafeProg = unsafeParse progT
formTable = M.fromList.map toTriple
unsafeInitTable = unsafeRight.initTable

derefAll []             = []
derefAll (FormT i e:fs) = let form = FormT i (derefOne fs e):derefAll fs
                          in  if any hasVarF form then derefAll form else form

hasVarF (FormT _ e)    = hasVar e
hasVarP (PairT _ e)    = hasVar e
hasVar (FuncT _ _ es)  = any hasVar es
hasVar (ArrT _ _ es)   = any hasVar es
hasVar (ObjT _ _ ps)   = any hasVarP ps
hasVar (VarT _)        = True
hasVar _               = False

derefOne fs (FuncT w m es)     = FuncT w m  $ map (derefOne fs) es
derefOne fs (ArrT p w es)      = ArrT p w $ map (derefOne fs) es
derefOne fs (ObjT p w ps)      = ObjT p w   $ map (derefP fs) ps
derefOne fs (VarT (IdT _ _ m)) = fromMaybe (error $ "Couldn't find var ["++m++"] in prog ["++show fs++"]") $ lookup m $ map toTuple fs
derefOne _  e                  = e
derefP   fs (PairT n e)        = PairT n $ derefOne fs e

nullGuard xs ifNull action = if null xs then return ifNull else action

toTuple (FormT (IdT _ _ a) b) = (a,b)
toTriple(FormT (IdT p _ a) b) = (a,(b,p))
formName = fst.toTuple
formPos = snd.snd.toTriple

diff x = filter (/= x)

type ChooseString m = ([String] -> m String)
type ChooseForm m   = ([FormToken] -> m FormToken)

{-| Monomorphism restriction -}
mUniqueDefs :: Monad m => m ProgTA     -> m UniqueDefs
mMultiDefs  :: Monad m => m UniqueDefs -> m MultiDefs
mValidVars  :: Monad m => m UniqueDefs -> ChooseString m -> m ValidVars
mUndefVars  :: Monad m => m ValidVars  -> ChooseString m -> ChooseForm m -> m UndefVars
mCycleVars  :: Monad m => m ValidVars  -> ChooseString m -> m CycleVars

mValidFuncs  :: Monad m => m ValidVars  -> m ValidFuncs
mUndefFuncs  :: Monad m => m ValidFuncs -> ChooseString m -> ChooseForm m -> m UndefFuncs
mNoShowFuncs :: Monad m => m ValidFuncs -> m NoShowFuncs





















