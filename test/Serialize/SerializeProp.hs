{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Serialize.SerializeProp where

import Test.Framework

import Serialize.SerializePropUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_Parse            (ParseS            expect input) = serializeCase expect input Left
prop_MultDef          (MultDefS          expect input) = serializeCase expect input Left
prop_UndefVar         (UndefVarS         expect input) = serializeCase expect input Left
prop_Cycle            (CycleS            expect input) = serializeCase expect input Left
prop_UndefFunc        (UndefFuncS        expect input) = serializeCase expect input Left
prop_NonTopShow       (NonTopShowS       expect input) = serializeCase expect input Left
prop_NoShow           (NoShowS           expect input) = serializeCase expect input Left
prop_ArgCount         (ArgCountS         expect input) = serializeCase expect input Left
prop_TypeMismatch     (TypeMismatchS     expect input) = serializeCase expect input Left
prop_IllegalEmpty     (IllegalEmptyS     expect input) = serializeCase expect input Left
prop_TableColLen      (TableColLenS      expect input) = serializeCase expect input Left
prop_TableHeadLen     (TableHeadLenS     expect input) = serializeCase expect input Left
prop_TableTakeLen     (TableTakeLenS     expect input) = serializeCase expect input Left
prop_IndexOutOfBounds (IndexOutOfBoundsS expect input) = serializeCase expect input Left
prop_Table (TableS expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Plot  (PlotS  expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Arr   (ArrS   expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Obj   (ObjS   expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Str   (StrS   expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Num   (NumS   expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Bool  (BoolS  expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right
prop_Null  (NullS  expA inA expB inB) = serializeCase expA inA Right && serializeCase expB inB Right

           