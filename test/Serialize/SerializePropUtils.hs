{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Serialize.SerializePropUtils where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.EvalError
import Data.ExpObj
import Data.ExpToken
import Data.List
import Data.Type
import Test.Framework

import Marshall.MarshallPropUtils
import Parser.ParserPropUtils
import Serialize.SerializeUtils

data ParseS = ParseS String EvalError deriving (Show)
instance Arbitrary ParseS where
  arbitrary                             = mParseS  arbitrary   arbitrary
  shrink (ParseS _ (InvalidParse p ls)) = mParseS (tShrink p) (tShrinks ls)
mParseS pa lsa = do p  <- liftM un pa; ls <- liftM (map un) lsa; return $ ParseS ("{_type:ERROR,_errType:PARSE,_pos:"++posStr p++",_data:"++show ls++"}") $ InvalidParse p ls 

data MultDefS = MultDefS String EvalError deriving (Show)
instance Arbitrary MultDefS where
  arbitrary                                     = mMultDefS  arbitrary   arbitrary
  shrink (MultDefS _ (MultipleDefinitions p s)) = mMultDefS (tShrink p) (tShrink s)
mMultDefS pa sa = do p  <- liftM un pa; s <- liftM un sa; return $ MultDefS ("{_type:ERROR,_errType:MULT_DEFS,_pos:"++posStr p++",_data:"++show s++"}") $ MultipleDefinitions p s 

data UndefVarS = UndefVarS String EvalError deriving (Show)
instance Arbitrary UndefVarS where
  arbitrary                                      = mUndefVarS  arbitrary   arbitrary
  shrink (UndefVarS _ (MultipleDefinitions p s)) = mUndefVarS (tShrink p) (tShrink s)
mUndefVarS pa sa = do p  <- liftM un pa; s <- liftM un sa; return $ UndefVarS ("{_type:ERROR,_errType:UNDEF_VAR,_pos:"++posStr p++",_data:"++show s++"}") $ UndefinedVariable p s 

data CycleS = CycleS String EvalError deriving (Show)
instance Arbitrary CycleS where
  arbitrary                                 = mCycleS  arbitrary  
  shrink (CycleS _ (CycleInDefinitions ps)) = mCycleS (tShrinks ps)
mCycleS psa = do ps  <- liftM (map un) psa; return $ CycleS ("{_type:ERROR,_errType:CYCLE,_data:"++showCycles ps++"}") $ CycleInDefinitions ps
showCycles ps = "[" ++ intercalate "," (map showCycle ps) ++ "]"
showCycle  ((x,y),n) = "[[" ++ show x ++ "," ++ show y ++ "]," ++ show n ++ "]"
instance Unto (P,AsciiStr) (Pos,String) where un (p,s) = (un p,un s); to (p,s) = (to p, to s)

data UndefFuncS = UndefFuncS String EvalError deriving (Show)
instance Arbitrary UndefFuncS where
  arbitrary                                       = mUndefFuncS  arbitrary   arbitrary
  shrink (UndefFuncS _ (MultipleDefinitions p s)) = mUndefFuncS (tShrink p) (tShrink s)
mUndefFuncS pa sa = do p  <- liftM un pa; s <- liftM un sa; return $ UndefFuncS ("{_type:ERROR,_errType:UNDEF_FUNC,_pos:"++posStr p++",_data:"++show s++"}") $ UndefinedFunction p s 

data NonTopShowS = NonTopShowS String EvalError deriving (Show)
instance Arbitrary NonTopShowS where
  arbitrary                                  = mNonTopShowS  arbitrary
  shrink (NonTopShowS _ (NonTopLevelShow p)) = mNonTopShowS (tShrink p)
mNonTopShowS pa = do p <- liftM un pa; return $ NonTopShowS ("{_type:ERROR,_errType:NON_TOP_SHOW,_pos:"++posStr p++"}") $ NonTopLevelShow p 

data NoShowS = NoShowS String EvalError deriving (Show)
instance Arbitrary NoShowS where arbitrary = return $ NoShowS "{_type:ERROR,_errType:NO_SHOW}" NoShow 

data ArgCountS = ArgCountS String EvalError deriving (Show)
instance Arbitrary ArgCountS where
  arbitrary                                       = mArgCountS  arbitrary   arbitrary  arbitrary  arbitrary
  shrink (ArgCountS _ (ArgCountMismatch p s e a)) = mArgCountS (tShrink p) (tShrink s) (shrink e) (shrink a)
mArgCountS pa sa ea aa = do 
  p  <- liftM un pa; s <- liftM un sa; e <- ea; a <- aa; 
  return $ ArgCountS ("{_type:ERROR,_errType:ARG_COUNT,_pos:"++posStr p++",_func:"++show s++",_exp:"++show e++",_act:"++show a++"}") $ ArgCountMismatch p s e a 

data TypeTreeA = TypeTreeA TypeTree deriving (Show)
instance Unto TypeTreeA TypeTree where to = TypeTreeA; un (TypeTreeA t) = t
instance Arbitrary TypeTreeA where arbitrary = sized1 tall; shrink (TypeTreeA t) = mTypeTreeA $ shrinkTypeTree t
instance Tall      TypeTreeA where                                        tall n = mTypeTreeA $ randomTypeTree n

randomTypeTree 0              = elements basicTypeTrees
randomTypeTree n              = do ts <- sListOf $ randomTypeTree $ n-1; elements (NodeOr ts:basicTypeTrees)

shrinkTypeTree (NodeOr ts) = liftM (NodeOr .map un) (tShrinks ts :: [[TypeTreeA]]) ++ ts 
shrinkTypeTree _           = []

mTypeTreeA = liftM TypeTreeA
basicTypeTrees = [LeafTable,LeafPlot,NodeArr,NodeObj,LeafStr,LeafNum,LeafBool,LeafNull]

data TypeTreeS = TypeTreeS TypeTree deriving (Show)
instance Unto TypeTreeS TypeTree where to = TypeTreeS; un (TypeTreeS t) = t
instance Arbitrary TypeTreeS where arbitrary = liftM TypeTreeS $ elements basicTypeTrees

data TMTreeA = TMTreeA TMTree deriving (Show)
instance Unto TMTreeA TMTree where to = TMTreeA; un (TMTreeA t) = t
instance Arbitrary TMTreeA where arbitrary = sized1 tall; shrink (TMTreeA t) = mTMTreeA $ shrinkTree t
instance Tall      TMTreeA where                                      tall n = mTMTreeA $ randomTree n
randomTree 0              = mTMLeaf arbitrary arbitrary arbitrary
randomTree n              = mTMNode $ talls $ n-1

shrinkTree (TMLeaf p e a) = mTMLeaf (tShrink p) (tShrink e) (tShrink a) 
shrinkTree (TMNode ts)    = mTMNode $ tShrinks ts

mTMTreeA = liftM TMTreeA 
mTMLeaf  = liftMF3 TMLeaf un un un
mTMNode  = liftM (TMNode .map un)

data TypeMismatchS = TypeMismatchS String EvalError deriving (Show)
instance Arbitrary TypeMismatchS where arbitrary = sized1 tall; shrink (TypeMismatchS _ (TypeMismatch t)) = mTypeMismatchS (tShrink t)
instance Tall      TypeMismatchS where                                                             tall n = mTypeMismatchS (tall n)
mTypeMismatchS ta = do t <- liftM un ta; return $ TypeMismatchS ("{_type:ERROR,_errType:TYPE_MISMATCH,_data:"++showType t++"}") $ TypeMismatch t
showType (TMNode ts)    = "{_type:TM_NODE,_data:["++intercalate "," (map showType ts)++"]}"
showType (TMLeaf p e a) = "{_type:TM_LEAF,_pos:"++posStr p++",_exp:"++showTypeTree e++",_act:"++showTypeTree a++"}"

showTypeTree LeafTable   = show "Table"
showTypeTree LeafPlot    = show "Plot"
showTypeTree NodeArr     = show "Array"
showTypeTree NodeObj     = show "Object"
showTypeTree LeafStr     = show "String"
showTypeTree LeafNum     = show "Number"
showTypeTree LeafBool    = show "Boolean"
showTypeTree LeafNull    = show "Null"
showTypeTree (NodeOr ts) = "{_type:OR_TYPE,_data:["++intercalate "," (map showTypeTree ts)++"]}"

data IllegalEmptyS = IllegalEmptyS String EvalError deriving (Show)
instance Arbitrary IllegalEmptyS where
  arbitrary                                 = mIllegalEmptyS  arbitrary
  shrink (IllegalEmptyS _ (IllegalEmpty p)) = mIllegalEmptyS (tShrink p)
mIllegalEmptyS pa = do p <- liftM un pa; return $ IllegalEmptyS ("{_type:ERROR,_errType:ILLEGAL_EMPTY,_pos:"++posStr p++"}") $ IllegalEmpty p 

data TableColLenS = TableColLenS String EvalError deriving (Show)
instance Arbitrary TableColLenS where
  arbitrary                                                 = mTableColLenS  arbitrary  arbitrary  arbitrary
  shrink (TableColLenS _ (TableColumnLengthMismatch p e a)) = mTableColLenS (tShrink p) (shrink e) (shrink a)
mTableColLenS pa ea aa = do 
  (p,e,a) <- getPea pa ea aa
  return $ TableColLenS ("{_type:ERROR,_errType:TABLE_COL_LEN,_pos:"++posStr p++",_exp:"++show e++",_act:"++show a++"}") $ TableColumnLengthMismatch p e a

data TableHeadLenS = TableHeadLenS String EvalError deriving (Show)
instance Arbitrary TableHeadLenS where
  arbitrary                                                  = mTableHeadLenS  arbitrary  arbitrary  arbitrary
  shrink (TableHeadLenS _ (TableHeaderLengthMismatch p e a)) = mTableHeadLenS (tShrink p) (shrink e) (shrink a)
mTableHeadLenS pa ea aa = do 
  (p,e,a) <- getPea pa ea aa
  return $ TableHeadLenS ("{_type:ERROR,_errType:TABLE_HEAD_LEN,_pos:"++posStr p++",_exp:"++show e++",_act:"++show a++"}") $ TableHeaderLengthMismatch p e a

data TableTakeLenS = TableTakeLenS String EvalError deriving (Show)
instance Arbitrary TableTakeLenS where
  arbitrary                                               = mTableTakeLenS  arbitrary  arbitrary  arbitrary
  shrink (TableTakeLenS _ (IllegalTakeTableLength p e a)) = mTableTakeLenS (tShrink p) (shrink e) (shrink a)
mTableTakeLenS pa ea aa = do 
  p  <- liftM un pa; e <- ea; a <- aa; 
  return $ TableTakeLenS ("{_type:ERROR,_errType:TABLE_TAKE_LEN,_pos:"++posStr p++",_exp:"++show e++",_act:"++show a++"}") $ IllegalTakeTableLength p e a

data IndexOutOfBoundsS = IndexOutOfBoundsS String EvalError deriving (Show)
instance Arbitrary IndexOutOfBoundsS where
  arbitrary                                                 = mIndexOutOfBoundsS  arbitrary   arbitrary   arbitrary  arbitrary
  shrink (IndexOutOfBoundsS _ (IndexOutOfBounds p mi ma a)) = mIndexOutOfBoundsS (tShrink p) (shrink mi) (shrink ma) (shrink a)
mIndexOutOfBoundsS pa mia maa aa = do 
  p  <- liftM un pa; mi <- mia; ma <- maa; a <- aa; 
  return $ IndexOutOfBoundsS ("{_type:ERROR,_errType:INDEX_OUT_OF_BOUNDS,_pos:"++posStr p++",_min:"++show mi++",_max:"++show ma++",_act:"++show a++"}") $ IndexOutOfBounds p mi ma a

data TableS = TableS String ExpObj String ExpObj deriving (Show)
instance Arbitrary TableS where arbitrary = sized1 tall; shrink (TableS _ a _ b) = mTableS (tShrink a) (tShrink b)
instance Tall      TableS where                                           tall n = mTableS (tall n)    (tall n)
mTableS ca ua = do
  c@(TableO _ essc hc) <- liftM un ca
  u@(TableO _ essu hu) <- liftM un ua
  return $ TableS ("{_type:TABLE,_data:"++showColumns essc ++",_head:"++showElems hc++"}") c ("{_type:UPD,_pos:"++getPosStr u++",_val:{_type:TABLE,_data:"++showColumns essu++",_head:"++showElems hu++"}}") u
showColumns ess = "[" ++ intercalate "," (map showColumn ess)            ++ "]"
showColumn es   = "[" ++ intercalate "," (map (serializeValid.Right) es) ++ "]"

data PlotS = PlotS String ExpObj String ExpObj deriving (Show)
instance Arbitrary PlotS where arbitrary = sized1 tall; shrink (PlotS _ a _ b) = mPlotS (tShrink a) (tShrink b)
instance Tall      PlotS where                                          tall n = mPlotS (tall n)    (tall n)
mPlotS ca ua = do
  c@(PlotO _ psc hc) <- liftM un ca
  u@(PlotO _ psu hu) <- liftM un ua
  return $ PlotS ("{_type:PLOT,_data:"++showPoints psc++",_head:"++showPairs hc++"}") c ("{_type:UPD,_pos:"++getPosStr u++",_val:{_type:PLOT,_data:"++showPoints psu++",_head:"++showPairs hu++"}}") u
showPoints ps   = "[" ++ intercalate "," (map showPoint ps)                   ++ "]"
showPoint (x,y) = "[" ++ serializeValid (Right x)++","++serializeValid (Right y)++ "]"

data ArrS = ArrS String ExpObj String ExpObj deriving (Show)
instance Arbitrary ArrS where arbitrary = sized1 tall; shrink (ArrS _ a _ b) = mArrS (tShrink a) (tShrink b)
instance Tall      ArrS where                                         tall n = mArrS (tall n)    (tall n)
mArrS ca ua = do
  c@(ArrO _ vc) <- liftM un ca
  u@(ArrO _ vu) <- liftM un ua
  return $ ArrS (showElems vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++showElems vu++"}") u
showElems es = "[" ++ intercalate "," (map (serializeValid.Right) es) ++ "]"

data ObjS = ObjS String ExpObj String ExpObj deriving (Show)
instance Arbitrary ObjS where arbitrary = sized1 tall; shrink (ObjS _ a _ b) = mObjS (tShrink a) (tShrink b)
instance Tall      ObjS where                                         tall n = mObjS (tall n)    (tall n)
mObjS ca ua = do
  c@(ObjO _ vc) <- liftM un ca
  u@(ObjO _ vu) <- liftM un ua
  return $ ObjS (showPairs vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++showPairs vu++"}") u
showPairs ps = "{" ++ intercalate "," (map showPair ps) ++ "}"
showPair (k,o) = show k ++ ":" ++ serializeValid (Right o)

data StrS = StrS String ExpObj String ExpObj deriving (Show)
instance Arbitrary StrS where
  arbitrary             = mStrS  arbitrary   arbitrary 
  shrink (StrS _ a _ b) = mStrS (tShrink a) (tShrink b)
mStrS ca ua = do
  c@(StrO _ vc) <- liftM un ca
  u@(StrO _ vu) <- liftM un ua
  return $ StrS (show vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++show vu++"}") u

data NumS = NumS String ExpObj String ExpObj deriving (Show)
instance Arbitrary NumS where
  arbitrary             = mNumS  arbitrary   arbitrary 
  shrink (NumS _ a _ b) = mNumS (tShrink a) (tShrink b)
mNumS ca ua = do
  c@(NumO _ vc) <- liftM un ca
  u@(NumO _ vu) <- liftM un ua
  return $ NumS (show vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++show vu++"}") u

data BoolS = BoolS String ExpObj String ExpObj deriving (Show)
instance Arbitrary BoolS where
  arbitrary              = mBoolS  arbitrary   arbitrary 
  shrink (BoolS _ a _ b) = mBoolS (tShrink a) (tShrink b)
mBoolS ca ua = do
  c@(BoolO _ vc) <- liftM un ca
  u@(BoolO _ vu) <- liftM un ua
  let toVal = map toLower . show
  return $ BoolS (toVal vc) c ("{_type:UPD,_pos:"++getPosStr u++",_val:"++toVal vu++"}") u
 
data NullS = NullS String ExpObj String ExpObj deriving (Show)
instance Arbitrary NullS where
  arbitrary              = mNullS  arbitrary   arbitrary 
  shrink (NullS _ a _ b) = mNullS (tShrink a) (tShrink b)
mNullS ca ua = do
  c <- liftM un ca
  u <- liftM un ua
  return $ NullS "null" c ("{_type:UPD,_pos:"++getPosStr u++",_val:null}") u

getPosStr (TableO p _ _) = oPosStr p
getPosStr (PlotO  p _ _) = oPosStr p
getPosStr (ArrO   p _)   = oPosStr p
getPosStr (ObjO   p _)   = oPosStr p
getPosStr (StrO   p _)   = oPosStr p
getPosStr (NumO   p _)   = oPosStr p
getPosStr (BoolO  p _)   = oPosStr p
getPosStr (NullO  p)     = oPosStr p

oPosStr (Upd p) = posStr p
oPosStr x       = "SerializeProp::posStr [Unexpected pattern ["++show x++"]]"

posStr (x,y) = show [x,y]

serializeCase expect input f = expect == serializeValid (f input)

getPea pa ea aa = do p <- pa; e <- ea; a <- aa; return (un p,e,a)

{-| Mandatory type signatures -}
mParseS            :: (Applicative m,Monad m) => m P -> m [AsciiStr]                           -> m ParseS
mMultDefS          :: (Applicative m,Monad m) => m P -> m  AsciiStr                            -> m MultDefS
mUndefVarS         :: (Applicative m,Monad m) => m P -> m  AsciiStr                            -> m UndefVarS
mCycleS            :: (Applicative m,Monad m) =>                       m [(P,AsciiStr)]        -> m CycleS
mUndefFuncS        :: (Applicative m,Monad m) => m P -> m  AsciiStr                            -> m UndefFuncS
mNonTopShowS       :: (Applicative m,Monad m) => m P                                           -> m NonTopShowS
mArgCountS         :: (Applicative m,Monad m) => m P -> m  AsciiStr -> m Int -> m Int          -> m ArgCountS
mTypeMismatchS     :: (Applicative m,Monad m) =>                       m TMTreeA               -> m TypeMismatchS
mIllegalEmptyS     :: (Applicative m,Monad m) => m P                                           -> m IllegalEmptyS
mTableColLenS      :: (Applicative m,Monad m) => m P                -> m Int -> m Int          -> m TableColLenS
mTableHeadLenS     :: (Applicative m,Monad m) => m P                -> m Int -> m Int          -> m TableHeadLenS
mTableTakeLenS     :: (Applicative m,Monad m) => m P                -> m Int -> m Int          -> m TableTakeLenS
mIndexOutOfBoundsS :: (Applicative m,Monad m) => m P                -> m Int -> m Int -> m Int -> m IndexOutOfBoundsS


mTableS :: (Applicative m,Monad m) => m (TablePA COP) -> m (TablePA UOP) -> m TableS
mPlotS  :: (Applicative m,Monad m) => m (PlotPA  COP) -> m (PlotPA  UOP) -> m PlotS
mArrS   :: (Applicative m,Monad m) => m (ArrPA   COP) -> m (ArrPA   UOP) -> m ArrS
mObjS   :: (Applicative m,Monad m) => m (ObjPA   COP) -> m (ObjPA   UOP) -> m ObjS
mStrS   :: (Applicative m,Monad m) => m (StrPA   COP) -> m (StrPA   UOP) -> m StrS
mNumS   :: (Applicative m,Monad m) => m (NumPA   COP) -> m (NumPA   UOP) -> m NumS
mBoolS  :: (Applicative m,Monad m) => m (BoolPA  COP) -> m (BoolPA  UOP) -> m BoolS
mNullS  :: (Applicative m,Monad m) => m (NullPA  COP) -> m (NullPA  UOP) -> m NullS

mTypeTreeA :: (Applicative m,Monad m) => m TypeTree -> m TypeTreeA
mTMTreeA   :: (Applicative m,Monad m) => m TMTree -> m TMTreeA
mTMLeaf    :: (Applicative m,Monad m) => m P -> m TypeTreeA -> m TypeTreeS -> m TMTree
mTMNode    :: (Applicative m,Monad m) => m [TMTreeA]                       -> m TMTree
