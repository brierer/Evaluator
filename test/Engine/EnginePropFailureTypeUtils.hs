module Engine.EnginePropFailureTypeUtils where

import Control.Monad.State
import Data.EvalError
import Data.ExpToken
import Data.ExpObj
import Data.HasPos
import Eval.Marshall
import Eval.MatchType
import Test.Framework

import Engine.EnginePropFailureUtils
import Parser.ParserUnitUtils
import MatchType.MatchTypeUnitUtils

data Show1 = Show1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Show1 where arbitrary = do ((e,o),fs) <- runNoCall notArrExp; return $ Show1 e o fs

data Show2 = Show2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Show2 where arbitrary = do ((a,o),fs) <- runNoCall $ arrExpOf showableExp notShowableExp; return $ Show2 a o fs

data Multi1 = Multi1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Multi1 where arbitrary = do ((e,o),fs) <- runNoCall notArrExp; return $ Multi1 e o fs

data Multi2 = Multi2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Multi2 where arbitrary = do ((a,o),fs) <- runNoCall $ arrExpOf atomExp notAtomExp; return $ Multi2 a o fs

data Mean1 = Mean1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Mean1 where arbitrary = do ((e,o),fs) <- runNoCall notArrExp; return $ Mean1 e o fs

data Mean2 = Mean2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Mean2 where arbitrary = do ((a,o),fs) <- runNoCall $ arrExpOf atomExp notAtomExp; return $ Mean2 a o fs

data Desc1 = Desc1 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Desc1 where arbitrary = do ((e,o),fs) <- runNoCall notArrExp; return $ Desc1 e o fs

data Desc2 = Desc2 ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Desc2 where arbitrary = do ((a,o),fs) <- runNoCall $ arrExpOf atomExp notAtomExp; return $ Desc2 a o fs

data Table1 = Table1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runNoCall $ sequence [notArrExp,anyExp]; return $ Table1 e1 e2 o1 fs

data Table2 = Table2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table2 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runNoCall $ sequence [arrExpOf (arrExpOf' atomExp) notArrExp,anyExp]; return $ Table2 e1 e2 o1 fs

data Table3 = Table3 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table3 where
  arbitrary = do
    ((a1,o1),fs0) <- runNoCall $ arrExpOf atomExp notAtomExp
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [arrExpOf (arrExpOf' atomExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1)),anyExp]) fs0
    return $ Table3 e1 e2 o1 fs1

data Table4 = Table4 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table4 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [arrExpOf' $ arrExpOf' atomExp,notObjExp]; return $ Table4 e1 e2 o2 fs

data Table5 = Table5 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table5 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [arrExpOf' $ arrExpOf' atomExp,objExpOf (arrExpOf' strExp) notArrExp]; return $ Table5 e1 e2 o2 fs

data Table6 = Table6 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Table6 where
  arbitrary = do
    ((a1,o1),fs0) <- runNoCall $ arrExpOf strExp notStrExp
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [arrExpOf' $ arrExpOf' atomExp, objExpOf (arrExpOf' strExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1))]) fs0
    return $ Table6 e1 e2 o1 fs1

data NTimes1 = NTimes1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runNoCall $ sequence [notNumExp,anyExp]; return $ NTimes1 e1 e2 o1 fs

data NTimes2 = NTimes2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary NTimes2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [numExp,notNumExp]; return $ NTimes2 e1 e2 o2 fs

data Take1 = Take1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Take1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runNoCall $ sequence [notNumExp,anyExp]; return $ Take1 e1 e2 o1 fs

data Take2 = Take2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Take2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [numExp, chooseExp [plotExp,objExp,strExp,numExp,boolExp,nullExp]]; return $ Take2 e1 e2 o2 fs

data Sort1 = Sort1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runNoCall $ sequence [notNumExp,anyExp]; return $ Sort1 e1 e2 o1 fs

data Sort2 = Sort2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [numExp, chooseExp [plotExp,objExp,strExp,numExp,boolExp,nullExp]]; return $ Sort2 e1 e2 o2 fs

data Col1 = Col1 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col1 where arbitrary = do ([(e1,o1),(e2,_)],fs) <- runNoCall $ sequence [notNumExp,anyExp]; return $ Col1 e1 e2 o1 fs

data Col2 = Col2 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col2 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [numExp, chooseExp [plotExp,objExp,strExp,numExp,boolExp,nullExp]]; return $ Col2 e1 e2 o2 fs

data Plot1 = Plot1 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot1 where arbitrary = do ([(e1,o1),(e2,_),(e3,_)],fs) <- runNoCall $ sequence [notArrExp,anyExp,anyExp]; return $ Plot1 e1 e2 e3 o1 fs

data Plot2 = Plot2 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot2 where arbitrary = do ([(e1,o1),(e2,_),(e3,_)],fs) <- runNoCall $ sequence [arrExpOf numExp notNumExp,anyExp,anyExp]; return $ Plot2 e1 e2 e3 o1 fs

data Plot3 = Plot3 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot3 where arbitrary = do ([(e1,_),(e2,o2),(e3,_)],fs) <- runNoCall $ sequence [arrExpOf' numExp,notArrExp,anyExp]; return $ Plot3 e1 e2 e3 o2 fs

data Plot4 = Plot4 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot4 where arbitrary = do ([(e1,_),(e2,o2),(e3,_)],fs) <- runNoCall $ sequence [arrExpOf' numExp,arrExpOf numExp notNumExp,anyExp]; return $ Plot4 e1 e2 e3 o2 fs

data Plot5 = Plot5 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot5 where arbitrary = do ([(e1,_),(e2,_),(e3,o3)],fs) <- runNoCall $ sequence [arrExpOf' numExp,arrExpOf' numExp,notObjExp]; return $ Plot5 e1 e2 e3 o3 fs

data Plot6 = Plot6 ExpToken ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Plot6 where arbitrary = do ([(e1,_),(e2,_),(e3,o3)],fs) <- runNoCall $ sequence [arrExpOf' numExp,arrExpOf' numExp,objExpOf strExp notStrExp]; return $ Plot6 e1 e2 e3 o3 fs

data Sort3 = Sort3 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort3 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [numExp, arrExpOf (arrExpOf' atomExp) notArrExp]; return $ Sort3 e1 e2 o2 fs

data Sort4 = Sort4 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Sort4 where 
  arbitrary = do
    ((a1,o1),fs0) <- runNoCall $ arrExpOf atomExp notAtomExp
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [numExp, arrExpOf (arrExpOf' atomExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1))]) fs0
    return $ Sort4 e1 e2 o1 fs1

data Col3 = Col3 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col3 where arbitrary = do ([(e1,_),(e2,o2)],fs) <- runNoCall $ sequence [numExp, arrExpOf (arrExpOf' atomExp) notArrExp]; return $ Col3 e1 e2 o2 fs

data Col4 = Col4 ExpToken ExpToken ExpObj [FuncEntryShow] deriving (Show)
instance Arbitrary Col4 where 
  arbitrary = do
    ((a1,o1),fs0) <- runNoCall $ arrExpOf atomExp notAtomExp
    ([(e1,_),(e2,_)],fs1) <- runStateT (sequence [numExp, arrExpOf (arrExpOf' atomExp) (return (a1,unsafeMarshall (toFuncEntries fs0) a1))]) fs0
    return $ Col4 e1 e2 o1 fs1

typeCase e t n args fs  = (Left $ TypeMismatch $ TMLeaf (getPos e) t (getRoot e)) == marshallWith (mkFunc n args) (toFuncEntries fs)

tokPos (FuncT _ (IdT p _ _) _) = p
tokPos (ArrT  p _ _)           = p
tokPos (ObjT  p _ _)           = p
tokPos (StrT  p _ _)           = p
tokPos (NumT  p _ _ _)         = p
tokPos (BoolT p _ _)           = p
tokPos (NullT p _)             = p
