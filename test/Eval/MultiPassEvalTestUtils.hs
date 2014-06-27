{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Eval.MultiPassEvalTestUtils where

import qualified Data.Map as M             (fromList)
import qualified Data.Set as S             (empty,toList,insert)
                                           
import Data.Eval                           (Table)
import Data.Function                       (on)
import Data.List                           (nubBy,sort,nub)
import Data.Maybe                          (fromMaybe)
import Data.Token                          (ProgToken(..),FormToken(..),PairToken(..),IdToken(..),ExpToken(..),Pos)
import Control.Applicative                 ((<$>),(<*>))
import Control.Monad                       (liftM,zipWithM)
import Control.Monad.State                 (State,runState,get,put)
import Eval.MultiPass                      (initTable,formVal,pairVal,mapPair,mapMPair)
import Parser.MonolithicParserTestUtils    (ProgTA(..),sShrink)
import Test.Framework                      (Arbitrary,arbitrary,shrink,elements)
import Text.ParserCombinators.Parsec.Error (ParseError,errorMessages)

instance Eq ParseError where (==) a b = errorMessages a == errorMessages b

class HasProg a where
  forms     :: a -> [FormToken]
  fromForms :: [FormToken] -> a
  toToken   :: a -> ProgToken
  toToken = fromForms.forms

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
mMultiDefs pa = let empty = MultiDefs (fromForms []) p0 "" in do
  fs <- liftM forms pa
  nullGuard fs empty $ do
    let (FormT _ (IdT p _ n) _) = head fs
        fs' = FormT p0 (IdT p0 w2 n) (NullT p0 w2):tail fs 
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
replaceAllVars f (FormT p n v:fs) = do
  v' <- replaceVars (map formName fs) f v
  rest <- replaceAllVars f fs
  return $ FormT p n v': rest

replaceVars ns f (FuncT p w n es)     = liftM (FuncT p w n) $ mapM (replaceVars ns f) es
replaceVars ns f (ArrayT p w es)      = liftM (ArrayT p w)  $ mapM (replaceVars ns f) es
replaceVars ns f (ObjT p w ps)        = liftM (ObjT p w)    $ mapM (mapMPair $ replaceVars ns f) ps
replaceVars [] _ (VarT p (IdT _ w _)) = return (NullT p w)
replaceVars ns f (VarT p (IdT q w _)) = liftM (VarT p.IdT q w) $ f ns
replaceVars _  _ e                    = return e

data UndefVars = UndefVars ValidVars Pos String deriving (Show)
instance Arbitrary UndefVars where
  arbitrary                   = mUndefVars arbitrary                           elements elements
  shrink (UndefVars prog _ _) = mUndefVars (sShrink $ removeUnderscoresV prog) id       id
mUndefVars pa f1 f2 = let empty = UndefVars (fromForms []) p0 "" in do
  fs <- liftM forms pa
  let ns = getRefed fs
  nullGuard ns empty $ do 
    n <- f1 ns
    let fs' = filter (usesVar n) fs
    nullGuard fs' empty $ do
      moo <- f2 fs'
      let n' = n ++ "_"
          (f,p) = flip runState p0 $ replaceOneVar n n' moo
          result = map (\f' -> if formName f == formName f' then f else f') fs
      return $ UndefVars (fromForms result) p n'

removeUnderscoresV = fromForms.map f.forms where
  f (FormT p n e)         = FormT p n $ g e
  g (FuncT p w i es)      = FuncT p w i $ map g es
  g (ArrayT p w es)       = ArrayT p w  $ map g es
  g (ObjT p w ps)         = ObjT p w $ map (mapPair g) ps
  g (VarT p (IdT q w' n)) = VarT p (IdT q w' (filter (/='_') n))
  g e                     = e

replaceOneVar :: String -> String -> FormToken -> State Pos FormToken
replaceOneVar fn n' (FormT p n e) = liftM (FormT p n) $ replaceOneVarE fn n' e

replaceOneVarE :: String -> String -> ExpToken -> State Pos ExpToken
replaceOneVarE fn n' (FuncT p w i es)     = liftM (FuncT p w i) $ mapM (replaceOneVarE fn n') es
replaceOneVarE fn n' (ArrayT p w es)      = liftM (ArrayT p w) $ mapM (replaceOneVarE fn n') es
replaceOneVarE fn n' (ObjT p w ps)        = liftM (ObjT p w)   $ mapM (mapMPair $ replaceOneVarE fn n') ps
replaceOneVarE fn n' (VarT p (IdT q w n)) = liftM (VarT p . IdT q w) $ getNewName fn n n' q
replaceOneVarE _  _  e                    = return e

getNewName fn n n' q = do
  oldP <- get
  let (newN,newP) = case (fn == n,oldP == p0) of (True,True) -> (n',q); (True,_) -> (n',oldP); _ -> (n,oldP)
  put newP
  return newN

usesVar :: String -> FormToken -> Bool
usesVar fn (FormT _ _ e) = usesVarE fn e

usesVarE :: String -> ExpToken -> Bool
usesVarE fn (FuncT _ _ _ es)     = any (usesFuncE fn) es
usesVarE fn (ArrayT _ _ es)      = any (usesFuncE fn) es
usesVarE fn (ObjT _ _ ps)        = any (usesFuncE fn.pairVal) ps
usesVarE fn (VarT _ (IdT _ _ n)) = fn == n   
usesVarE _  _                    = False
    
getRefed = S.toList . f S.empty
  where f acc []                   = acc
        f acc (FormT _ _ e:fs)     = f (g acc e) fs
        g acc (FuncT _ _ _ es)     = foldl g acc es
        g acc (ArrayT _ _ es)      = foldl g acc es
        g acc (ObjT _ _ ps)        = foldl g acc $ map pairVal ps
        g acc (VarT _ (IdT _ _ n)) = S.insert n acc
        g acc _                    = acc

data CycleVars = CycleVars ValidVars [(Pos,String)] deriving (Show)
instance Arbitrary CycleVars where
  arbitrary                 = mCycleVars arbitrary      elements
  shrink (CycleVars prog _) = mCycleVars (sShrink prog) id
mCycleVars pa f = let empty = CycleVars (fromForms []) [] in do
  fs <- liftM (filter hasVarF.forms) pa
  nullGuard fs empty $ do
    let ns = map formName fs
    fs' <- zipWithM (makeCycle f) fs $ tail $ cycle ns
    return $ CycleVars (fromForms fs') (flipZip $ sort $ zip ns $ map formPos fs) where flipZip = uncurry (flip zip).unzip
makeCycle f (FormT p n e) m = liftM (FormT p n) $ replaceVars [m] f e

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
  f (FuncT _ _ (IdT _ _ n) es) = n:funcNames es
  f (ArrayT _ _ es)            = funcNames es
  f (ObjT _ _ ps)              = funcNames $ map pairVal ps
  f _                          = []

insertTopShow fs = FormT p0 (IdT p0 w2 "show") (FuncT p0"" (IdT p0 w2 "show") $ map formVal fs):fs

replaceNonTopShows = map f where 
  f (FormT p n e)               = FormT p n $ g e
  g (FuncT p w (IdT q w' n) es) = FuncT p w (IdT q w' (case n of "show" -> "notShow"; _ -> n)) $ map g es
  g (ArrayT p w es)             = ArrayT p w $ map g es
  g (ObjT p w ps)               = ObjT p w $ map (mapPair g) ps
  g e                           = e  
  
removeTopShow []                                                           = []
removeTopShow (FormT _ (IdT _ _ "show") (FuncT _ _ (IdT _ _ "show") _):fs) = fs
removeTopShow (f:fs)                                                       = f:removeTopShow fs

data UndefFuncs = UndefFuncs ValidFuncs Pos String deriving (Eq,Show)
instance Arbitrary UndefFuncs where
  arbitrary                      =          mUndefFuncs arbitrary                          elements    elements
  shrink p@(UndefFuncs prog _ _) = diff p $ mUndefFuncs (sShrink $ removeUnderscores prog) tail        tail
mUndefFuncs pa f1 f2 = let empty = UndefFuncs (fromForms []) p0 "" in do
  ValidFuncs prog fns <- pa
  nullGuard fns empty $ do
    fn <- f1 fns
    let fs = forms prog
        fn' = fn ++ "_"
    moo <- f2 $ filter (usesFunc fn) fs
    let (f,p) = flip runState p0 $ replaceOneFunc fn fn' moo
        fs' = map (\f' -> if formName f == formName f' then f else f') fs
    return $ UndefFuncs (ValidFuncs (fromForms fs') fns) p fn'

removeUnderscores = fromForms.map f.forms where
  f (FormT p n e)               = FormT p n $ g e
  g (FuncT p w (IdT q w' n) es) = FuncT p w (IdT q w' (filter (/='_') n)) $ map g es
  g (ArrayT p w es)             = ArrayT p w $ map g es
  g (ObjT p w ps)               = ObjT p w $ map (mapPair g) ps
  g e                           = e

replaceOneFunc :: String -> String -> FormToken -> State Pos FormToken
replaceOneFunc fn n' (FormT p n e) = liftM (FormT p n) $ replaceOneFuncE fn n' e

replaceOneFuncE :: String -> String -> ExpToken -> State Pos ExpToken
replaceOneFuncE fn n' (FuncT p w (IdT q w' n) es) = getNewName fn n n' q >>= \newN -> liftM (FuncT p w (IdT q w' newN)) $ mapM (replaceOneFuncE fn n') es
replaceOneFuncE fn n' (ArrayT p w es)             = liftM (ArrayT p w) $ mapM (replaceOneFuncE fn n') es
replaceOneFuncE fn n' (ObjT p w ps)               = liftM (ObjT p w)   $ mapM (mapMPair $ replaceOneFuncE fn n') ps
replaceOneFuncE _  _  e                           = return e

usesFunc :: String -> FormToken -> Bool
usesFunc fn (FormT _ _ e) = usesFuncE fn e

usesFuncE :: String -> ExpToken -> Bool
usesFuncE fn (FuncT _ _ (IdT _ _ n) es) = fn == n || any (usesFuncE fn) es
usesFuncE fn (ArrayT _ _ es)            =            any (usesFuncE fn) es
usesFuncE fn (ObjT _ _ ps)              =            any (usesFuncE fn.pairVal) ps
usesFuncE _  _                          = False

data NonTopShowFuncs = NonTopShowFuncs ValidFuncs Pos deriving (Eq,Show)
instance Arbitrary NonTopShowFuncs where
  arbitrary                         =          mNonTopShowFuncs arbitrary                    elements    elements
  shrink p@(NonTopShowFuncs prog _) = diff p $ mNonTopShowFuncs (sShrink $ removeShows prog) tail        tail
mNonTopShowFuncs pa f1 f2 = let empty = NonTopShowFuncs (fromForms []) p0 in do
  ValidFuncs prog fns <- pa
  nullGuard fns empty $ do
    fn <- f1 fns
    let fs = forms prog
    moo <- f2 $ filter (usesFuncNonTop fn) fs
    let (f,p) = flip runState p0 $ replaceOneNonTopFunc fn "show" moo
        fs' = map (\f' -> if formName f == formName f' then f else f') fs
    return $ NonTopShowFuncs (ValidFuncs (fromForms fs') fns) p

removeShows = fromForms.map f.forms where
  f (FormT p n (FuncT q w m es)) = FormT p n $ FuncT q w m $ map g es
  f (FormT p n e)                = FormT p n $ g e
  g (FuncT p w (IdT q w' n) es)  = FuncT p w (IdT q w' (if n == "show" then "notShow" else n)) $ map g es
  g (ArrayT p w es)              = ArrayT p w $ map g es
  g (ObjT p w ps)                = ObjT p w $ map (mapPair g) ps
  g e                            = e
  
replaceOneNonTopFunc :: String -> String -> FormToken -> State Pos FormToken
replaceOneNonTopFunc fn n' (FormT p n (FuncT q w m es)) = liftM (FormT p n .FuncT q w m) $ mapM (replaceOneFuncE fn n') es
replaceOneNonTopFunc fn n' (FormT p n e)                = liftM (FormT p n) $ replaceOneFuncE fn n' e

usesFuncNonTop :: String -> FormToken -> Bool
usesFuncNonTop fn (FormT _ _ (FuncT _ _ _ es)) = any (usesFuncE fn) es
usesFuncNonTop fn (FormT _ _ e)                = usesFuncE fn e

data NoShowFuncs = NoShowFuncs ValidFuncs deriving (Eq,Show)
instance HasProg NoShowFuncs where
  forms (NoShowFuncs prog) = forms prog
  fromForms = NoShowFuncs .fromForms
instance Arbitrary NoShowFuncs where
  arbitrary                 = mNoShowFuncs arbitrary
  shrink (NoShowFuncs prog) = mNoShowFuncs (sShrink prog)
mNoShowFuncs = liftM (fromForms.removeTopShow.forms)

{- | Utils -}
fromProgForms = M.fromList.map toTriple.forms
derefValidProg = fromForms.derefAll.forms
derefValidProg' = initTable'.derefValidProg
initTable' = (\(Right x)->x).initTable.toToken

derefAll []             = []
derefAll (FormT p n v:fs) = let moo1 = FormT p n (derefOne fs v):derefAll fs
                            in  if any hasVarF moo1 then derefAll moo1 else moo1

hasVarF (FormT _ _ e) = hasVar e
hasVarP (PairT _ _ e) = hasVar e
hasVar (FuncT _ _ _ es) = any hasVar es
hasVar (ArrayT _ _ es)  = any hasVar es
hasVar (ObjT _ _ ps)    = any hasVarP ps
hasVar (VarT _ _)       = True
hasVar _                = False

derefOne fs (FuncT p w m es)     = FuncT p w m  $ map (derefOne fs) es
derefOne fs (ArrayT p w es)      = ArrayT p w   $ map (derefOne fs) es
derefOne fs (ObjT p w ps)        = ObjT p w     $ map (derefP fs) ps
derefOne fs (VarT _ (IdT _ _ m)) = fromMaybe (error $ "Couldn't find var ["++m++"] in prog ["++show fs++"]") $ lookup m $ map toTuple fs
derefOne _  e                    = e
derefP   fs (PairT p n e)        = PairT p n $ derefOne fs e

nullGuard xs ifNull action = if null xs then return ifNull else action

toTuple (FormT _ (IdT _ _ a) b) = (a,b)
toTriple(FormT _ (IdT p _ a) b) = (a,(b,p))
formName = fst.toTuple
formPos = snd.snd.toTriple

nonEmpty = not.null.forms

diff x = filter (/= x)
w2 = ("","")
p0 = (0,0)

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

derefValidProg  :: HasProg a => a -> a
derefValidProg' :: HasProg a => a -> Table
fromProgForms   :: HasProg a => a -> Table
initTable'      :: HasProg a => a -> Table
nonEmpty        :: HasProg a => a -> Bool




















