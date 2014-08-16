{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Serialize.SerializeProp where

import Test.Framework

import Serialize.SerializePropUtils
import Serialize.SerializeUtils

import Data.ExpObj

{-# ANN module "HLint: ignore Use camelCase" #-}

--test_Error = do assertEqual "{_type:ERROR,_errType:PARSE,_pos:[0,0],_data:[]}"                                                      $ serializeValid $ Left $ InvalidParse (0,0) []
--                assertEqual "{_type:ERROR,_errType:PARSE,_pos:[1,1],_data:[\"1\",\"2\",\"3\"]}"                                     $ serializeValid $ Left $ InvalidParse (1,1) ["1","2","3"]
--
--                assertEqual "{_type:ERROR,_errType:MULT_DEFS,_pos:[0,0],_data:\"\"}"                                                $ serializeValid $ Left $ MultipleDefinitions (0,0) ""
--                assertEqual "{_type:ERROR,_errType:MULT_DEFS,_pos:[1,1],_data:\"funcName\"}"                                        $ serializeValid $ Left $ MultipleDefinitions (1,1) "funcName"
--
--                assertEqual "{_type:ERROR,_errType:UNDEF_VAR,_pos:[0,0],_data:\"\"}"                                                $ serializeValid $ Left $ UndefinedVariable (0,0) ""
--                assertEqual "{_type:ERROR,_errType:UNDEF_VAR,_pos:[1,1],_data:\"varName\"}"                                         $ serializeValid $ Left $ UndefinedVariable (1,1) "varName"
--
--                assertEqual "{_type:ERROR,_errType:CYCLE,_data:[]}"                                                                 $ serializeValid $ Left $ CycleInDefinitions []
--                assertEqual "{_type:ERROR,_errType:CYCLE,_data:[[[1,2],\"x\"]]}"                                                    $ serializeValid $ Left $ CycleInDefinitions [((1,2),"x")]
--                assertEqual "{_type:ERROR,_errType:CYCLE,_data:[[[3,4],\"y\"],[[5,6],\"z\"]]}"                                      $ serializeValid $ Left $ CycleInDefinitions [((3,4),"y"),((5,6),"z")]
--
--                assertEqual "{_type:ERROR,_errType:UNDEF_FUNC,_pos:[0,0],_data:\"\"}"                                               $ serializeValid $ Left $ UndefinedFunction (0,0) ""
--                assertEqual "{_type:ERROR,_errType:UNDEF_FUNC,_pos:[1,1],_data:\"funcName\"}"                                       $ serializeValid $ Left $ UndefinedFunction (1,1) "funcName"
--
--                assertEqual "{_type:ERROR,_errType:NON_TOP_SHOW,_pos:[0,1]}"                                                        $ serializeValid $ Left $ NonTopLevelShow (0,1)
--                assertEqual "{_type:ERROR,_errType:NON_TOP_SHOW,_pos:[2,3]}"                                                        $ serializeValid $ Left $ NonTopLevelShow (2,3)
--
--                assertEqual "{_type:ERROR,_errType:NO_SHOW}"                                                                        $ serializeValid $ Left NoShow
--
--                assertEqual "{_type:ERROR,_errType:ARG_COUNT,_pos:[0,0],_func:\"\",_exp:0,_act:0}"                                  $ serializeValid $ Left $ ArgCountMismatch (0,0) "" 0 0
--                assertEqual "{_type:ERROR,_errType:ARG_COUNT,_pos:[1,2],_func:\"func\",_exp:3,_act:4}"                              $ serializeValid $ Left $ ArgCountMismatch (1,2) "func" 3 4
--
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[0,0],_exp:\"Null\",_act:\"Boolean\"}}"  $ serializeValid $ Left $ TypeMismatch $ TMLeaf (0,0) LeafNull LeafBool
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[1,1],_exp:\"Number\",_act:\"String\"}}" $ serializeValid $ Left $ TypeMismatch $ TMLeaf (1,1) LeafNum  LeafStr
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[2,2],_exp:\"Object\",_act:\"Array\"}}"  $ serializeValid $ Left $ TypeMismatch $ TMLeaf (2,2) NodeObj  NodeArr
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[3,3],_exp:\"Plot\",_act:\"Table\"}}"    $ serializeValid $ Left $ TypeMismatch $ TMLeaf (3,3) LeafPlot LeafTable
--
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[0,0],_exp:{_type:OR_TYPE,_data:[\"Null\",\"Number\"]},_act:\"Boolean\"}}"  $ serializeValid $ Left $ TypeMismatch $ TMLeaf (0,0) (NodeOr [LeafNull,LeafNum])  LeafBool
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[1,1],_exp:{_type:OR_TYPE,_data:[\"Number\",\"Object\"]},_act:\"String\"}}" $ serializeValid $ Left $ TypeMismatch $ TMLeaf (1,1) (NodeOr [LeafNum,NodeObj])   LeafStr
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[2,2],_exp:{_type:OR_TYPE,_data:[\"Object\",\"Plot\"]},_act:\"Array\"}}"    $ serializeValid $ Left $ TypeMismatch $ TMLeaf (2,2) (NodeOr [NodeObj,LeafPlot])  NodeArr
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[3,3],_exp:{_type:OR_TYPE,_data:[\"Plot\",\"Null\"]},_act:\"Table\"}}"      $ serializeValid $ Left $ TypeMismatch $ TMLeaf (3,3) (NodeOr [LeafPlot,LeafNull]) LeafTable
--
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[]}}"                                                           $ serializeValid $ Left $ TypeMismatch $ TMNode []
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[0,0],_exp:\"Null\",_act:\"Boolean\"}]}}"  $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (0,0) LeafNull LeafBool]
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[1,1],_exp:\"Number\",_act:\"String\"}]}}" $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (1,1) LeafNum  LeafStr]
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[2,2],_exp:\"Object\",_act:\"Array\"}]}}"  $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (2,2) NodeObj  NodeArr]
--                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[3,3],_exp:\"Plot\",_act:\"Table\"}]}}"    $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (3,3) LeafPlot LeafTable]
--
--                -- Expected type: Table | ArrOf (Null | Boolean) | ArrOf Str, Actual type: ArrOf (ArrOf Obj)
--                assertEqual ("{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:["++
--                  "{_type:TM_LEAF,_pos:[0,0],_exp:\"Table\",_act:\"Array\"},"++
--                  "{_type:TM_NODE,_data:["++
--                    "{_type:TM_LEAF,_pos:[1,1],_exp:{_type:OR_TYPE,_data:[\"Null\",\"Boolean\"]},_act:\"Array\"},"++
--                    "{_type:TM_LEAF,_pos:[2,2],_exp:\"String\",_act:\"Array\"}]},"++
--                  "{_type:TM_LEAF,_pos:[3,3],_exp:\"Number\",_act:\"Object\"}]}}")
--                    $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (0,0) LeafTable NodeArr,
--                                                        TMNode [TMLeaf (1,1) (NodeOr [LeafNull,LeafBool]) NodeArr,
--                                                                TMLeaf (2,2) LeafStr NodeArr],
--                                                        TMLeaf (3,3) LeafNum NodeObj]
--
--                assertEqual "{_type:ERROR,_errType:ILLEGAL_EMPTY,_pos:[0,1]}"                            $ serializeValid $ Left $ IllegalEmpty (0,1)
--                assertEqual "{_type:ERROR,_errType:ILLEGAL_EMPTY,_pos:[2,3]}"                            $ serializeValid $ Left $ IllegalEmpty (2,3)
--
--                assertEqual "{_type:ERROR,_errType:TABLE_COL_LEN,_pos:[0,1],_exp:2,_act:3}"              $ serializeValid $ Left $ TableColumnLengthMismatch (0,1) 2 3
--                assertEqual "{_type:ERROR,_errType:TABLE_COL_LEN,_pos:[4,5],_exp:6,_act:7}"              $ serializeValid $ Left $ TableColumnLengthMismatch (4,5) 6 7
--
--                assertEqual "{_type:ERROR,_errType:TABLE_HEAD_LEN,_pos:[0,1],_exp:2,_act:3}"             $ serializeValid $ Left $ TableHeaderLengthMismatch (0,1) 2 3
--                assertEqual "{_type:ERROR,_errType:TABLE_HEAD_LEN,_pos:[4,5],_exp:6,_act:7}"             $ serializeValid $ Left $ TableHeaderLengthMismatch (4,5) 6 7
--
--                assertEqual "{_type:ERROR,_errType:TABLE_TAKE_LEN,_pos:[0,1],_exp:2,_act:3}"             $ serializeValid $ Left $ IllegalTakeTableLength (0,1) 2 3
--                assertEqual "{_type:ERROR,_errType:TABLE_TAKE_LEN,_pos:[4,5],_exp:6,_act:7}"             $ serializeValid $ Left $ IllegalTakeTableLength (4,5) 6 7
--
--                assertEqual "{_type:ERROR,_errType:INDEX_OUT_OF_BOUNDS,_pos:[0,1],_min:2,_max:3,_act:4}" $ serializeValid $ Left $ IndexOutOfBounds (0,1) 2 3 4
--                assertEqual "{_type:ERROR,_errType:INDEX_OUT_OF_BOUNDS,_pos:[5,6],_min:7,_max:8,_act:9}" $ serializeValid $ Left $ IndexOutOfBounds (5,6) 7 8 9
--
prop_Table (TableS expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Plot  (PlotS  expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Arr   (ArrS   expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Obj   (ObjS   expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Str   (StrS   expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Num   (NumS   expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Bool  (BoolS  expA inA expB inB) = serializeCase expA inA expB inB Right
prop_Null  (NullS  expA inA expB inB) = serializeCase expA inA expB inB Right

           
           