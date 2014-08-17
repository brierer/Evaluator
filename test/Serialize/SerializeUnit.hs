{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Serialize.SerializeUnit where

import Data.EvalError
import Data.Type
import Test.Framework

import Marshall.MarshallUtils
import Serialize.SerializeUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Error = do assertEqual "{_type:ERROR,_errType:PARSE,_pos:[0,0],_data:[]}"                                                      $ serializeValid $ Left $ InvalidParse (0,0) []
                assertEqual "{_type:ERROR,_errType:PARSE,_pos:[1,1],_data:[\"1\",\"2\",\"3\"]}"                                     $ serializeValid $ Left $ InvalidParse (1,1) ["1","2","3"]

                assertEqual "{_type:ERROR,_errType:MULT_DEFS,_pos:[0,0],_data:\"\"}"                                                $ serializeValid $ Left $ MultipleDefinitions (0,0) ""
                assertEqual "{_type:ERROR,_errType:MULT_DEFS,_pos:[1,1],_data:\"funcName\"}"                                        $ serializeValid $ Left $ MultipleDefinitions (1,1) "funcName"

                assertEqual "{_type:ERROR,_errType:UNDEF_VAR,_pos:[0,0],_data:\"\"}"                                                $ serializeValid $ Left $ UndefinedVariable (0,0) ""
                assertEqual "{_type:ERROR,_errType:UNDEF_VAR,_pos:[1,1],_data:\"varName\"}"                                         $ serializeValid $ Left $ UndefinedVariable (1,1) "varName"

                assertEqual "{_type:ERROR,_errType:CYCLE,_data:[]}"                                                                 $ serializeValid $ Left $ CycleInDefinitions []
                assertEqual "{_type:ERROR,_errType:CYCLE,_data:[[[1,2],\"x\"]]}"                                                    $ serializeValid $ Left $ CycleInDefinitions [((1,2),"x")]
                assertEqual "{_type:ERROR,_errType:CYCLE,_data:[[[3,4],\"y\"],[[5,6],\"z\"]]}"                                      $ serializeValid $ Left $ CycleInDefinitions [((3,4),"y"),((5,6),"z")]

                assertEqual "{_type:ERROR,_errType:UNDEF_FUNC,_pos:[0,0],_data:\"\"}"                                               $ serializeValid $ Left $ UndefinedFunction (0,0) ""
                assertEqual "{_type:ERROR,_errType:UNDEF_FUNC,_pos:[1,1],_data:\"funcName\"}"                                       $ serializeValid $ Left $ UndefinedFunction (1,1) "funcName"

                assertEqual "{_type:ERROR,_errType:NON_TOP_SHOW,_pos:[0,1]}"                                                        $ serializeValid $ Left $ NonTopLevelShow (0,1)
                assertEqual "{_type:ERROR,_errType:NON_TOP_SHOW,_pos:[2,3]}"                                                        $ serializeValid $ Left $ NonTopLevelShow (2,3)

                assertEqual "{_type:ERROR,_errType:NO_SHOW}"                                                                        $ serializeValid $ Left NoShow

                assertEqual "{_type:ERROR,_errType:ARG_COUNT,_pos:[0,0],_func:\"\",_exp:0,_act:0}"                                  $ serializeValid $ Left $ ArgCountMismatch (0,0) "" 0 0
                assertEqual "{_type:ERROR,_errType:ARG_COUNT,_pos:[1,2],_func:\"func\",_exp:3,_act:4}"                              $ serializeValid $ Left $ ArgCountMismatch (1,2) "func" 3 4

                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[0,0],_exp:\"Null\",_act:\"Boolean\"}}"  $ serializeValid $ Left $ TypeMismatch $ TMLeaf (0,0) LeafNull LeafBool
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[1,1],_exp:\"Number\",_act:\"String\"}}" $ serializeValid $ Left $ TypeMismatch $ TMLeaf (1,1) LeafNum  LeafStr
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[2,2],_exp:\"Object\",_act:\"Array\"}}"  $ serializeValid $ Left $ TypeMismatch $ TMLeaf (2,2) NodeObj  NodeArr
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[3,3],_exp:\"Plot\",_act:\"Table\"}}"    $ serializeValid $ Left $ TypeMismatch $ TMLeaf (3,3) LeafPlot LeafTable

                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[0,0],_exp:{_type:OR_TYPE,_data:[\"Null\",\"Number\"]},_act:\"Boolean\"}}"  $ serializeValid $ Left $ TypeMismatch $ TMLeaf (0,0) (NodeOr [LeafNull,LeafNum])  LeafBool
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[1,1],_exp:{_type:OR_TYPE,_data:[\"Number\",\"Object\"]},_act:\"String\"}}" $ serializeValid $ Left $ TypeMismatch $ TMLeaf (1,1) (NodeOr [LeafNum,NodeObj])   LeafStr
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[2,2],_exp:{_type:OR_TYPE,_data:[\"Object\",\"Plot\"]},_act:\"Array\"}}"    $ serializeValid $ Left $ TypeMismatch $ TMLeaf (2,2) (NodeOr [NodeObj,LeafPlot])  NodeArr
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_LEAF,_pos:[3,3],_exp:{_type:OR_TYPE,_data:[\"Plot\",\"Null\"]},_act:\"Table\"}}"      $ serializeValid $ Left $ TypeMismatch $ TMLeaf (3,3) (NodeOr [LeafPlot,LeafNull]) LeafTable

                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[]}}"                                                           $ serializeValid $ Left $ TypeMismatch $ TMNode []
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[0,0],_exp:\"Null\",_act:\"Boolean\"}]}}"  $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (0,0) LeafNull LeafBool]
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[1,1],_exp:\"Number\",_act:\"String\"}]}}" $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (1,1) LeafNum  LeafStr]
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[2,2],_exp:\"Object\",_act:\"Array\"}]}}"  $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (2,2) NodeObj  NodeArr]
                assertEqual "{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:[{_type:TM_LEAF,_pos:[3,3],_exp:\"Plot\",_act:\"Table\"}]}}"    $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (3,3) LeafPlot LeafTable]

                -- Expected type: Table | ArrOf (Null | Boolean) | ArrOf Str, Actual type: ArrOf (ArrOf Obj)
                assertEqual ("{_type:ERROR,_errType:TYPE_MISMATCH,_data:{_type:TM_NODE,_data:["++
                  "{_type:TM_LEAF,_pos:[0,0],_exp:\"Table\",_act:\"Array\"},"++
                  "{_type:TM_NODE,_data:["++
                    "{_type:TM_LEAF,_pos:[1,1],_exp:{_type:OR_TYPE,_data:[\"Null\",\"Boolean\"]},_act:\"Array\"},"++
                    "{_type:TM_LEAF,_pos:[2,2],_exp:\"String\",_act:\"Array\"}]},"++
                  "{_type:TM_LEAF,_pos:[3,3],_exp:\"Number\",_act:\"Object\"}]}}")
                    $ serializeValid $ Left $ TypeMismatch $ TMNode [TMLeaf (0,0) LeafTable NodeArr,
                                                        TMNode [TMLeaf (1,1) (NodeOr [LeafNull,LeafBool]) NodeArr,
                                                                TMLeaf (2,2) LeafStr NodeArr],
                                                        TMLeaf (3,3) LeafNum NodeObj]

                assertEqual "{_type:ERROR,_errType:ILLEGAL_EMPTY,_pos:[0,1]}"                            $ serializeValid $ Left $ IllegalEmpty (0,1)
                assertEqual "{_type:ERROR,_errType:ILLEGAL_EMPTY,_pos:[2,3]}"                            $ serializeValid $ Left $ IllegalEmpty (2,3)

                assertEqual "{_type:ERROR,_errType:TABLE_COL_LEN,_pos:[0,1],_exp:2,_act:3}"              $ serializeValid $ Left $ TableColumnLengthMismatch (0,1) 2 3
                assertEqual "{_type:ERROR,_errType:TABLE_COL_LEN,_pos:[4,5],_exp:6,_act:7}"              $ serializeValid $ Left $ TableColumnLengthMismatch (4,5) 6 7

                assertEqual "{_type:ERROR,_errType:TABLE_HEAD_LEN,_pos:[0,1],_exp:2,_act:3}"             $ serializeValid $ Left $ TableHeaderLengthMismatch (0,1) 2 3
                assertEqual "{_type:ERROR,_errType:TABLE_HEAD_LEN,_pos:[4,5],_exp:6,_act:7}"             $ serializeValid $ Left $ TableHeaderLengthMismatch (4,5) 6 7

                assertEqual "{_type:ERROR,_errType:TABLE_TAKE_LEN,_pos:[0,1],_exp:2,_act:3}"             $ serializeValid $ Left $ IllegalTakeTableLength (0,1) 2 3
                assertEqual "{_type:ERROR,_errType:TABLE_TAKE_LEN,_pos:[4,5],_exp:6,_act:7}"             $ serializeValid $ Left $ IllegalTakeTableLength (4,5) 6 7

                assertEqual "{_type:ERROR,_errType:INDEX_OUT_OF_BOUNDS,_pos:[0,1],_min:2,_max:3,_act:4}" $ serializeValid $ Left $ IndexOutOfBounds (0,1) 2 3 4
                assertEqual "{_type:ERROR,_errType:INDEX_OUT_OF_BOUNDS,_pos:[5,6],_min:7,_max:8,_act:9}" $ serializeValid $ Left $ IndexOutOfBounds (5,6) 7 8 9

test_Table = do assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[]}"                                                              $ serializeValid $ Right $ mkTableC (1,2) [[mkStrC  (3,4) ""]] []
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[]}"                                                               $ serializeValid $ Right $ mkTableC (1,2) [[mkNumC  (3,4) 0.0]] []
                assertEqual "{_type:TABLE,_data:[[false]],_head:[]}"                                                             $ serializeValid $ Right $ mkTableC (1,2) [[mkBoolC (3,4) False]] []
                assertEqual "{_type:TABLE,_data:[[null]],_head:[]}"                                                              $ serializeValid $ Right $ mkTableC (1,2) [[mkNullC (3,4)]] []

                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[]}"                                  $ serializeValid $ Right $ mkTableC (1,2) [[mkStrU  (3,4) ""]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[]}"                                   $ serializeValid $ Right $ mkTableC (1,2) [[mkNumU  (3,4) 0.0]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[]}"                                 $ serializeValid $ Right $ mkTableC (1,2) [[mkBoolU (3,4) False]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[]}"                                  $ serializeValid $ Right $ mkTableC (1,2) [[mkNullU (3,4)]] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[]}}"                                  $ serializeValid $ Right $ mkTableU (1,2) [[mkStrC  (3,4) ""]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}"                                   $ serializeValid $ Right $ mkTableU (1,2) [[mkNumC  (3,4) 0.0]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[]}}"                                 $ serializeValid $ Right $ mkTableU (1,2) [[mkBoolC (3,4) False]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[]}}"                                  $ serializeValid $ Right $ mkTableU (1,2) [[mkNullC (3,4)]] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[]}}"      $ serializeValid $ Right $ mkTableU (1,2) [[mkStrU  (3,4) ""]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[]}}"       $ serializeValid $ Right $ mkTableU (1,2) [[mkNumU  (3,4) 0.0]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[]}}"     $ serializeValid $ Right $ mkTableU (1,2) [[mkBoolU (3,4) False]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[]}}"      $ serializeValid $ Right $ mkTableU (1,2) [[mkNullU (3,4)]] []

                assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[\"\"]}"                                                          $ serializeValid $ Right $ mkTableC (1,2) [[mkStrC  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[\"\"]}"                                                           $ serializeValid $ Right $ mkTableC (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[false]],_head:[\"\"]}"                                                         $ serializeValid $ Right $ mkTableC (1,2) [[mkBoolC (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[null]],_head:[\"\"]}"                                                          $ serializeValid $ Right $ mkTableC (1,2) [[mkNullC (3,4)]]       [mkStrC  (5,6) ""]

                assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                              $ serializeValid $ Right $ mkTableC (1,2) [[mkStrC  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                               $ serializeValid $ Right $ mkTableC (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[false]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                             $ serializeValid $ Right $ mkTableC (1,2) [[mkBoolC (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[null]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                              $ serializeValid $ Right $ mkTableC (1,2) [[mkNullC (3,4)]]       [mkStrU  (5,6) ""]

                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[\"\"]}"                              $ serializeValid $ Right $ mkTableC (1,2) [[mkStrU  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[\"\"]}"                               $ serializeValid $ Right $ mkTableC (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[\"\"]}"                             $ serializeValid $ Right $ mkTableC (1,2) [[mkBoolU (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[\"\"]}"                              $ serializeValid $ Right $ mkTableC (1,2) [[mkNullU (3,4)]]       [mkStrC  (5,6) ""]

                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"  $ serializeValid $ Right $ mkTableC (1,2) [[mkStrU  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"   $ serializeValid $ Right $ mkTableC (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}" $ serializeValid $ Right $ mkTableC (1,2) [[mkBoolU (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"  $ serializeValid $ Right $ mkTableC (1,2) [[mkNullU (3,4)]]       [mkStrU  (5,6) ""]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[\"\"]}}"                              $ serializeValid $ Right $ mkTableU (1,2) [[mkStrC  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[\"\"]}}"                               $ serializeValid $ Right $ mkTableU (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[\"\"]}}"                             $ serializeValid $ Right $ mkTableU (1,2) [[mkBoolC (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[\"\"]}}"                              $ serializeValid $ Right $ mkTableU (1,2) [[mkNullC (3,4)]]       [mkStrC  (5,6) ""]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"  $ serializeValid $ Right $ mkTableU (1,2) [[mkStrC  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"   $ serializeValid $ Right $ mkTableU (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}" $ serializeValid $ Right $ mkTableU (1,2) [[mkBoolC (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"  $ serializeValid $ Right $ mkTableU (1,2) [[mkNullC (3,4)]]       [mkStrU  (5,6) ""]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[\"\"]}}"  $ serializeValid $ Right $ mkTableU (1,2) [[mkStrU  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[\"\"]}}"   $ serializeValid $ Right $ mkTableU (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[\"\"]}}" $ serializeValid $ Right $ mkTableU (1,2) [[mkBoolU (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[\"\"]}}"  $ serializeValid $ Right $ mkTableU (1,2) [[mkNullU (3,4)]]       [mkStrC  (5,6) ""]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"    $ serializeValid $ Right $ mkTableU (1,2) [[mkStrU  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"     $ serializeValid $ Right $ mkTableU (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"   $ serializeValid $ Right $ mkTableU (1,2) [[mkBoolU (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"    $ serializeValid $ Right $ mkTableU (1,2) [[mkNullU (3,4)]]       [mkStrU  (5,6) ""]

test_Plot = do  assertEqual "{_type:PLOT,_data:[],_head:{}}"                                                                                                   $ serializeValid $ Right $ mkPlotC (1,2) [] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{}}}"                                                                       $ serializeValid $ Right $ mkPlotU (1,2) [] []

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{}}"                                                                                        $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{}}"                                                            $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}"                                                            $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}"                                $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{}}}"                                                            $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{}}}"                                $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}}"                                $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}}"    $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] []

                assertEqual "{_type:PLOT,_data:[],_head:{title:\"\"}}"                                                                                         $ serializeValid $ Right $ mkPlotC (1,2) [] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{title:\"\"}}}"                                                             $ serializeValid $ Right $ mkPlotU (1,2) [] [("title",mkStrC (7,8) "")]

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{title:\"\"}}"                                                                              $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}"                                                  $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:\"\"}}"                                                  $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}"                      $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{title:\"\"}}}"                                                         $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:\"\"}}}"                             $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}}"                             $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}}" $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]

                assertEqual "{_type:PLOT,_data:[],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                                                              $ serializeValid $ Right $ mkPlotC (1,2) [] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                                                                  $ serializeValid $ Right $ mkPlotU (1,2) [] [("title",mkStrU (7,8) "")]

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                                                   $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                       $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                       $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                           $ serializeValid $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                                                         $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                             $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                             $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}" $ serializeValid $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]

test_Arr = do   assertEqual "[{_type:TABLE,_data:[[0.0]],_head:[]}]"                            $ serializeValid $ Right $ mkArrC (1,2) [mkTableC (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "[{_type:PLOT,_data:[],_head:{}}]"                                  $ serializeValid $ Right $ mkArrC (1,2) [mkPlotC  (3,4) [] []]
                assertEqual "[[]]"                                                              $ serializeValid $ Right $ mkArrC (1,2) [mkArrC   (3,4) []]
                assertEqual "[{}]"                                                              $ serializeValid $ Right $ mkArrC (1,2) [mkObjC   (3,4) []]
                assertEqual "[\"\"]"                                                            $ serializeValid $ Right $ mkArrC (1,2) [mkStrC   (3,4) ""]
                assertEqual "[0.0]"                                                             $ serializeValid $ Right $ mkArrC (1,2) [mkNumC   (3,4) 0.0]
                assertEqual "[false]"                                                           $ serializeValid $ Right $ mkArrC (1,2) [mkBoolC  (3,4) False]
                assertEqual "[null]"                                                            $ serializeValid $ Right $ mkArrC (1,2) [mkNullC  (3,4)]

                assertEqual "[{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}]" $ serializeValid $ Right $ mkArrC (1,2) [mkTableU (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}]"       $ serializeValid $ Right $ mkArrC (1,2) [mkPlotU  (3,4) [] []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:[]}]"                                   $ serializeValid $ Right $ mkArrC (1,2) [mkArrU   (3,4) []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{}}]"                                   $ serializeValid $ Right $ mkArrC (1,2) [mkObjU   (3,4) []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:\"\"}]"                                 $ serializeValid $ Right $ mkArrC (1,2) [mkStrU   (3,4) ""]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:0.0}]"                                  $ serializeValid $ Right $ mkArrC (1,2) [mkNumU   (3,4) 0.0]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:false}]"                                $ serializeValid $ Right $ mkArrC (1,2) [mkBoolU  (3,4) False]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:null}]"                                 $ serializeValid $ Right $ mkArrC (1,2) [mkNullU  (3,4)]

                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:TABLE,_data:[[0.0]],_head:[]}]}" $ serializeValid $ Right $ mkArrU (1,2) [mkTableC (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:PLOT,_data:[],_head:{}}]}"       $ serializeValid $ Right $ mkArrU (1,2) [mkPlotC  (3,4) [] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[[]]}"                                   $ serializeValid $ Right $ mkArrU (1,2) [mkArrC   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{}]}"                                   $ serializeValid $ Right $ mkArrU (1,2) [mkObjC   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[\"\"]}"                                 $ serializeValid $ Right $ mkArrU (1,2) [mkStrC   (3,4) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[0.0]}"                                  $ serializeValid $ Right $ mkArrU (1,2) [mkNumC   (3,4) 0.0]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[false]}"                                $ serializeValid $ Right $ mkArrU (1,2) [mkBoolC  (3,4) False]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[null]}"                                 $ serializeValid $ Right $ mkArrU (1,2) [mkNullC  (3,4)]

                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}]}" $ serializeValid $ Right $ mkArrU (1,2) [mkTableU (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}]}"       $ serializeValid $ Right $ mkArrU (1,2) [mkPlotU  (3,4) [] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:[]}]}"                                   $ serializeValid $ Right $ mkArrU (1,2) [mkArrU   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{}}]}"                                   $ serializeValid $ Right $ mkArrU (1,2) [mkObjU   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:\"\"}]}"                                 $ serializeValid $ Right $ mkArrU (1,2) [mkStrU   (3,4) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:0.0}]}"                                  $ serializeValid $ Right $ mkArrU (1,2) [mkNumU   (3,4) 0.0]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:false}]}"                                $ serializeValid $ Right $ mkArrU (1,2) [mkBoolU  (3,4) False]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:null}]}"                                 $ serializeValid $ Right $ mkArrU (1,2) [mkNullU  (3,4)]

test_Obj = do   assertEqual "{_x:{_type:TABLE,_data:[[0.0]],_head:[]}}"                           $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkTableC (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{_x:{_type:PLOT,_data:[],_head:{}}}"                                 $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkPlotC  (3,4) [] [])]
                assertEqual "{_x:[]}"                                                             $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkArrC   (3,4) [])]
                assertEqual "{_x:{}}"                                                             $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkObjC   (3,4) [])]
                assertEqual "{_x:\"\"}"                                                           $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkStrC   (3,4) "")]
                assertEqual "{_x:0.0}"                                                            $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkNumC   (3,4) 0)]
                assertEqual "{_x:false}"                                                          $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkBoolC  (3,4) False)]
                assertEqual "{_x:null}"                                                           $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkNullC  (3,4))]

                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}}" $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkTableU (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}}"       $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkPlotU  (3,4) [] [])]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:[]}}"                                   $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkArrU   (3,4) [])]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:{}}}"                                   $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkObjU   (3,4) [])]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:\"\"}}"                                 $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkStrU   (3,4) "")]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:0.0}}"                                  $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkNumU   (3,4) 0)]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:false}}"                                $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkBoolU  (3,4) False)]
                assertEqual "{_x:{_type:UPD,_pos:[3,4],_val:null}}"                                 $ serializeValid $ Right $ mkObjC (1,2) [("_x",mkNullU  (3,4))]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:TABLE,_data:[[0.0]],_head:[]}}}" $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkTableC (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:PLOT,_data:[],_head:{}}}}"       $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkPlotC  (3,4) [] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:[]}}"                                   $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkArrC   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{}}}"                                   $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkObjC   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:\"\"}}"                                 $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkStrC   (3,4) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:0.0}}"                                  $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkNumC   (3,4) 0)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:false}}"                                $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkBoolC  (3,4) False)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:null}}"                                 $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkNullC  (3,4))]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}}}" $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkTableU (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}}}"       $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkPlotU  (3,4) [] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:[]}}}"                                   $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkArrU   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:{}}}}"                                   $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkObjU   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:\"\"}}}"                                 $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkStrU   (3,4) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:0.0}}}"                                  $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkNumU   (3,4) 0)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:false}}}"                                $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkBoolU  (3,4) False)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_x:{_type:UPD,_pos:[3,4],_val:null}}}"                                 $ serializeValid $ Right $ mkObjU (1,2) [("_x",mkNullU  (3,4))]

test_Str = do   assertEqual "\"0\""                             $ serializeValid $ Right $ mkStrC (0,0) "0"
                assertEqual "\"\""                              $ serializeValid $ Right $ mkStrC (0,0) ""

                assertEqual "{_type:UPD,_pos:[1,2],_val:\"0\"}" $ serializeValid $ Right $ mkStrU (1,2) "0"
                assertEqual "{_type:UPD,_pos:[1,2],_val:\"\"}"  $ serializeValid $ Right $ mkStrU (1,2) ""

test_Num = do   assertEqual "1.0"                               $ serializeValid $ Right $ mkNumC (0,0) 1
                assertEqual "0.0"                               $ serializeValid $ Right $ mkNumC (0,0) 0

                assertEqual "{_type:UPD,_pos:[1,2],_val:1.0}"   $ serializeValid $ Right $ mkNumU (1,2) 1
                assertEqual "{_type:UPD,_pos:[1,2],_val:0.0}"   $ serializeValid $ Right $ mkNumU (1,2) 0

test_Bool = do  assertEqual "true"                              $ serializeValid $ Right $ mkBoolC (0,0) True
                assertEqual "false"                             $ serializeValid $ Right $ mkBoolC (0,0) False

                assertEqual "{_type:UPD,_pos:[1,2],_val:true}"  $ serializeValid $ Right $ mkBoolU (1,2) True
                assertEqual "{_type:UPD,_pos:[1,2],_val:false}" $ serializeValid $ Right $ mkBoolU (1,2) False

test_Null = do  assertEqual "null"                              $ serializeValid $ Right $ mkNullC (0,0)
                assertEqual "{_type:UPD,_pos:[1,2],_val:null}"  $ serializeValid $ Right $ mkNullU (1,2)

