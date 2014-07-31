{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Serialize.SerializeUnit where

import Data.ExpObj
import Eval.Serialize

import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Table = do assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[]}"                                                              $ serialize $ TableO (Calc (1,2)) [[StrO  (Calc (3,4)) ""]] []
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[]}"                                                               $ serialize $ TableO (Calc (1,2)) [[NumO  (Calc (3,4)) 0.0]] []
                assertEqual "{_type:TABLE,_data:[[false]],_head:[]}"                                                             $ serialize $ TableO (Calc (1,2)) [[BoolO (Calc (3,4)) False]] []
                assertEqual "{_type:TABLE,_data:[[null]],_head:[]}"                                                              $ serialize $ TableO (Calc (1,2)) [[NullO (Calc (3,4))]] []

                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[]}"                                  $ serialize $ TableO (Calc (1,2)) [[StrO  (Upd  (3,4)) ""]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[]}"                                   $ serialize $ TableO (Calc (1,2)) [[NumO  (Upd  (3,4)) 0.0]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[]}"                                 $ serialize $ TableO (Calc (1,2)) [[BoolO (Upd  (3,4)) False]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[]}"                                  $ serialize $ TableO (Calc (1,2)) [[NullO (Upd  (3,4))]] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[]}}"                                  $ serialize $ TableO (Upd  (1,2)) [[StrO  (Calc (3,4)) ""]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}"                                   $ serialize $ TableO (Upd  (1,2)) [[NumO  (Calc (3,4)) 0.0]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[]}}"                                 $ serialize $ TableO (Upd  (1,2)) [[BoolO (Calc (3,4)) False]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[]}}"                                  $ serialize $ TableO (Upd  (1,2)) [[NullO (Calc (3,4))]] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[]}}"      $ serialize $ TableO (Upd  (1,2)) [[StrO  (Upd  (3,4)) ""]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[]}}"       $ serialize $ TableO (Upd  (1,2)) [[NumO  (Upd  (3,4)) 0.0]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[]}}"     $ serialize $ TableO (Upd  (1,2)) [[BoolO (Upd  (3,4)) False]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[]}}"      $ serialize $ TableO (Upd  (1,2)) [[NullO (Upd  (3,4))]] []

                assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[\"\"]}"                                                          $ serialize $ TableO (Calc (1,2)) [[StrO  (Calc (3,4)) ""]]    [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[\"\"]}"                                                           $ serialize $ TableO (Calc (1,2)) [[NumO  (Calc (3,4)) 0.0]]   [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[false]],_head:[\"\"]}"                                                         $ serialize $ TableO (Calc (1,2)) [[BoolO (Calc (3,4)) False]] [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[null]],_head:[\"\"]}"                                                          $ serialize $ TableO (Calc (1,2)) [[NullO (Calc (3,4))]]       [StrO  (Calc (5,6)) ""]
                                                                                                                                                                                                
                assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                              $ serialize $ TableO (Calc (1,2)) [[StrO  (Calc (3,4)) ""]]    [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                               $ serialize $ TableO (Calc (1,2)) [[NumO  (Calc (3,4)) 0.0]]   [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[false]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                             $ serialize $ TableO (Calc (1,2)) [[BoolO (Calc (3,4)) False]] [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[null]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                              $ serialize $ TableO (Calc (1,2)) [[NullO (Calc (3,4))]]       [StrO  (Upd  (5,6)) ""]
                                                                                                                                                                                                
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[\"\"]}"                              $ serialize $ TableO (Calc (1,2)) [[StrO  (Upd  (3,4)) ""]]    [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[\"\"]}"                               $ serialize $ TableO (Calc (1,2)) [[NumO  (Upd  (3,4)) 0.0]]   [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[\"\"]}"                             $ serialize $ TableO (Calc (1,2)) [[BoolO (Upd  (3,4)) False]] [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[\"\"]}"                              $ serialize $ TableO (Calc (1,2)) [[NullO (Upd  (3,4))]]       [StrO  (Calc (5,6)) ""]
                                                                                                                                                                                                
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"  $ serialize $ TableO (Calc (1,2)) [[StrO  (Upd  (3,4)) ""]]    [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"   $ serialize $ TableO (Calc (1,2)) [[NumO  (Upd  (3,4)) 0.0]]   [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}" $ serialize $ TableO (Calc (1,2)) [[BoolO (Upd  (3,4)) False]] [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"  $ serialize $ TableO (Calc (1,2)) [[NullO (Upd  (3,4))]]       [StrO  (Upd  (5,6)) ""]
                                                                                                                                                                                                
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[\"\"]}}"                              $ serialize $ TableO (Upd  (1,2)) [[StrO  (Calc (3,4)) ""]]    [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[\"\"]}}"                               $ serialize $ TableO (Upd  (1,2)) [[NumO  (Calc (3,4)) 0.0]]   [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[\"\"]}}"                             $ serialize $ TableO (Upd  (1,2)) [[BoolO (Calc (3,4)) False]] [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[\"\"]}}"                              $ serialize $ TableO (Upd  (1,2)) [[NullO (Calc (3,4))]]       [StrO  (Calc (5,6)) ""]
                                                                                                                                                                                                
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"  $ serialize $ TableO (Upd  (1,2)) [[StrO  (Calc (3,4)) ""]]    [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"   $ serialize $ TableO (Upd  (1,2)) [[NumO  (Calc (3,4)) 0.0]]   [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}" $ serialize $ TableO (Upd  (1,2)) [[BoolO (Calc (3,4)) False]] [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"  $ serialize $ TableO (Upd  (1,2)) [[NullO (Calc (3,4))]]       [StrO  (Upd  (5,6)) ""]
                                                                                                                                                                                                
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[\"\"]}}"  $ serialize $ TableO (Upd  (1,2)) [[StrO  (Upd  (3,4)) ""]]    [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[\"\"]}}"   $ serialize $ TableO (Upd  (1,2)) [[NumO  (Upd  (3,4)) 0.0]]   [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[\"\"]}}" $ serialize $ TableO (Upd  (1,2)) [[BoolO (Upd  (3,4)) False]] [StrO  (Calc (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[\"\"]}}"  $ serialize $ TableO (Upd  (1,2)) [[NullO (Upd  (3,4))]]       [StrO  (Calc (5,6)) ""]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"    $ serialize $ TableO (Upd  (1,2)) [[StrO  (Upd  (3,4)) ""]]    [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"     $ serialize $ TableO (Upd  (1,2)) [[NumO  (Upd  (3,4)) 0.0]]   [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"   $ serialize $ TableO (Upd  (1,2)) [[BoolO (Upd  (3,4)) False]] [StrO  (Upd  (5,6)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"    $ serialize $ TableO (Upd  (1,2)) [[NullO (Upd  (3,4))]]       [StrO  (Upd  (5,6)) ""]

test_Plot = do  assertEqual "{_type:PLOT,_data:[],_head:{}}"                                                                                                   $ serialize $ PlotO (Calc (1,2)) [] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{}}}"                                                                       $ serialize $ PlotO (Upd  (1,2)) [] []
                                                                                                                                                                 
                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{}}"                                                                                        $ serialize $ PlotO (Calc (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{}}"                                                            $ serialize $ PlotO (Calc (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}"                                                            $ serialize $ PlotO (Calc (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}"                                $ serialize $ PlotO (Calc (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] []
                                                                                                                                                               
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{}}}"                                                            $ serialize $ PlotO (Upd  (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{}}}"                                $ serialize $ PlotO (Upd  (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}}"                                $ serialize $ PlotO (Upd  (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}}"    $ serialize $ PlotO (Upd  (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] []
                                                                                                                                                               
                assertEqual "{_type:PLOT,_data:[],_head:{title:\"\"}}"                                                                                         $ serialize $ PlotO (Calc (1,2)) [] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{title:\"\"}}}"                                                             $ serialize $ PlotO (Upd  (1,2)) [] [("title",StrO (Calc (7,8)) "")]

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{title:\"\"}}"                                                                              $ serialize $ PlotO (Calc (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}"                                                  $ serialize $ PlotO (Calc (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:\"\"}}"                                                  $ serialize $ PlotO (Calc (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}"                      $ serialize $ PlotO (Calc (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{title:\"\"}}}"                                                         $ serialize $ PlotO (Upd  (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:\"\"}}}"                             $ serialize $ PlotO (Upd  (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}}"                             $ serialize $ PlotO (Upd  (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}}" $ serialize $ PlotO (Upd  (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Calc (7,8)) "")]

                assertEqual "{_type:PLOT,_data:[],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                                                              $ serialize $ PlotO (Calc (1,2)) [] [("title",StrO (Upd (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                                                                  $ serialize $ PlotO (Upd  (1,2)) [] [("title",StrO (Upd (7,8)) "")]

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                                                   $ serialize $ PlotO (Calc (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                       $ serialize $ PlotO (Calc (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                       $ serialize $ PlotO (Calc (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                           $ serialize $ PlotO (Calc (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                                                         $ serialize $ PlotO (Upd  (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                             $ serialize $ PlotO (Upd  (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Calc (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                             $ serialize $ PlotO (Upd  (1,2)) [(NumO (Calc (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}" $ serialize $ PlotO (Upd  (1,2)) [(NumO (Upd  (3,4)) 10.0,NumO (Upd  (5,6)) 11.0)] [("title",StrO (Upd  (7,8)) "")]

test_Arr = do   assertEqual "[{_type:TABLE,_data:[[0.0]],_head:[]}]"                            $ serialize $ ArrO (Calc (1,2)) [TableO (Calc (3,4)) [[NumO  (Calc (0,0)) 0.0]] []]
                assertEqual "[{_type:PLOT,_data:[],_head:{}}]"                                  $ serialize $ ArrO (Calc (1,2)) [PlotO  (Calc (3,4)) [] []]
                assertEqual "[[]]"                                                              $ serialize $ ArrO (Calc (1,2)) [ArrO   (Calc (3,4)) []]
                assertEqual "[{}]"                                                              $ serialize $ ArrO (Calc (1,2)) [ObjO   (Calc (3,4)) []]
                assertEqual "[\"\"]"                                                            $ serialize $ ArrO (Calc (1,2)) [StrO   (Calc (3,4)) ""]
                assertEqual "[0.0]"                                                             $ serialize $ ArrO (Calc (1,2)) [NumO   (Calc (3,4)) 0.0]
                assertEqual "[false]"                                                           $ serialize $ ArrO (Calc (1,2)) [BoolO  (Calc (3,4)) False]
                assertEqual "[null]"                                                            $ serialize $ ArrO (Calc (1,2)) [NullO  (Calc (3,4))]
                                                                                                                                        
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}]" $ serialize $ ArrO (Calc (1,2)) [TableO (Upd  (3,4)) [[NumO  (Calc (0,0)) 0.0]] []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}]"       $ serialize $ ArrO (Calc (1,2)) [PlotO  (Upd  (3,4)) [] []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:[]}]"                                   $ serialize $ ArrO (Calc (1,2)) [ArrO   (Upd  (3,4)) []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{}}]"                                   $ serialize $ ArrO (Calc (1,2)) [ObjO   (Upd  (3,4)) []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:\"\"}]"                                 $ serialize $ ArrO (Calc (1,2)) [StrO   (Upd  (3,4)) ""]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:0.0}]"                                  $ serialize $ ArrO (Calc (1,2)) [NumO   (Upd  (3,4)) 0.0]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:false}]"                                $ serialize $ ArrO (Calc (1,2)) [BoolO  (Upd  (3,4)) False]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:null}]"                                 $ serialize $ ArrO (Calc (1,2)) [NullO  (Upd  (3,4))]
                                                                                                                                        
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:TABLE,_data:[[0.0]],_head:[]}]}" $ serialize $ ArrO (Upd  (1,2)) [TableO (Calc (3,4)) [[NumO  (Calc (0,0)) 0.0]] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:PLOT,_data:[],_head:{}}]}"       $ serialize $ ArrO (Upd  (1,2)) [PlotO  (Calc (3,4)) [] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[[]]}"                                   $ serialize $ ArrO (Upd  (1,2)) [ArrO   (Calc (3,4)) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{}]}"                                   $ serialize $ ArrO (Upd  (1,2)) [ObjO   (Calc (3,4)) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[\"\"]}"                                 $ serialize $ ArrO (Upd  (1,2)) [StrO   (Calc (3,4)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[0.0]}"                                  $ serialize $ ArrO (Upd  (1,2)) [NumO   (Calc (3,4)) 0.0]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[false]}"                                $ serialize $ ArrO (Upd  (1,2)) [BoolO  (Calc (3,4)) False]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[null]}"                                 $ serialize $ ArrO (Upd  (1,2)) [NullO  (Calc (3,4))]
                                                                                                                                        
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}]}" $ serialize $ ArrO (Upd  (1,2)) [TableO (Upd  (3,4)) [[NumO  (Calc (0,0)) 0.0]] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}]}"       $ serialize $ ArrO (Upd  (1,2)) [PlotO  (Upd  (3,4)) [] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:[]}]}"                                   $ serialize $ ArrO (Upd  (1,2)) [ArrO   (Upd  (3,4)) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{}}]}"                                   $ serialize $ ArrO (Upd  (1,2)) [ObjO   (Upd  (3,4)) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:\"\"}]}"                                 $ serialize $ ArrO (Upd  (1,2)) [StrO   (Upd  (3,4)) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:0.0}]}"                                  $ serialize $ ArrO (Upd  (1,2)) [NumO   (Upd  (3,4)) 0.0]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:false}]}"                                $ serialize $ ArrO (Upd  (1,2)) [BoolO  (Upd  (3,4)) False]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:null}]}"                                 $ serialize $ ArrO (Upd  (1,2)) [NullO  (Upd  (3,4))]
                
test_Obj = do   assertEqual "{x:{_type:TABLE,_data:[[0.0]],_head:[]}}"                           $ serialize $ ObjO (Calc (1,2)) [("x",TableO (Calc (3,4)) [[NumO  (Calc (0,0)) 0.0]] [])]
                assertEqual "{x:{_type:PLOT,_data:[],_head:{}}}"                                 $ serialize $ ObjO (Calc (1,2)) [("x",PlotO  (Calc (3,4)) [] [])]
                assertEqual "{x:[]}"                                                             $ serialize $ ObjO (Calc (1,2)) [("x",ArrO   (Calc (3,4)) [])]
                assertEqual "{x:{}}"                                                             $ serialize $ ObjO (Calc (1,2)) [("x",ObjO   (Calc (3,4)) [])]
                assertEqual "{x:\"\"}"                                                           $ serialize $ ObjO (Calc (1,2)) [("x",StrO   (Calc (3,4)) "")]
                assertEqual "{x:0.0}"                                                            $ serialize $ ObjO (Calc (1,2)) [("x",NumO   (Calc (3,4)) 0)]
                assertEqual "{x:false}"                                                          $ serialize $ ObjO (Calc (1,2)) [("x",BoolO  (Calc (3,4)) False)]
                assertEqual "{x:null}"                                                           $ serialize $ ObjO (Calc (1,2)) [("x",NullO  (Calc (3,4)))]
                                                                                                                                              
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}}" $ serialize $ ObjO (Calc (1,2)) [("x",TableO (Upd  (3,4)) [[NumO  (Calc (0,0)) 0.0]] [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}}"       $ serialize $ ObjO (Calc (1,2)) [("x",PlotO  (Upd  (3,4)) [] [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:[]}}"                                   $ serialize $ ObjO (Calc (1,2)) [("x",ArrO   (Upd  (3,4)) [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:{}}}"                                   $ serialize $ ObjO (Calc (1,2)) [("x",ObjO   (Upd  (3,4)) [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:\"\"}}"                                 $ serialize $ ObjO (Calc (1,2)) [("x",StrO   (Upd  (3,4)) "")]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:0.0}}"                                  $ serialize $ ObjO (Calc (1,2)) [("x",NumO   (Upd  (3,4)) 0)]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:false}}"                                $ serialize $ ObjO (Calc (1,2)) [("x",BoolO  (Upd  (3,4)) False)]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:null}}"                                 $ serialize $ ObjO (Calc (1,2)) [("x",NullO  (Upd  (3,4)))]
                                                                                                                                              
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:TABLE,_data:[[0.0]],_head:[]}}}" $ serialize $ ObjO (Upd  (1,2)) [("x",TableO (Calc (3,4)) [[NumO  (Calc (0,0)) 0.0]] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:PLOT,_data:[],_head:{}}}}"       $ serialize $ ObjO (Upd  (1,2)) [("x",PlotO  (Calc (3,4)) [] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:[]}}"                                   $ serialize $ ObjO (Upd  (1,2)) [("x",ArrO   (Calc (3,4)) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{}}}"                                   $ serialize $ ObjO (Upd  (1,2)) [("x",ObjO   (Calc (3,4)) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:\"\"}}"                                 $ serialize $ ObjO (Upd  (1,2)) [("x",StrO   (Calc (3,4)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:0.0}}"                                  $ serialize $ ObjO (Upd  (1,2)) [("x",NumO   (Calc (3,4)) 0)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:false}}"                                $ serialize $ ObjO (Upd  (1,2)) [("x",BoolO  (Calc (3,4)) False)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:null}}"                                 $ serialize $ ObjO (Upd  (1,2)) [("x",NullO  (Calc (3,4)))]
                                                                                                                                              
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}}}" $ serialize $ ObjO (Upd  (1,2)) [("x",TableO (Upd  (3,4)) [[NumO  (Calc (0,0)) 0.0]] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}}}"       $ serialize $ ObjO (Upd  (1,2)) [("x",PlotO  (Upd  (3,4)) [] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:[]}}}"                                   $ serialize $ ObjO (Upd  (1,2)) [("x",ArrO   (Upd  (3,4)) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:{}}}}"                                   $ serialize $ ObjO (Upd  (1,2)) [("x",ObjO   (Upd  (3,4)) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:\"\"}}}"                                 $ serialize $ ObjO (Upd  (1,2)) [("x",StrO   (Upd  (3,4)) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:0.0}}}"                                  $ serialize $ ObjO (Upd  (1,2)) [("x",NumO   (Upd  (3,4)) 0)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:false}}}"                                $ serialize $ ObjO (Upd  (1,2)) [("x",BoolO  (Upd  (3,4)) False)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:null}}}"                                 $ serialize $ ObjO (Upd  (1,2)) [("x",NullO  (Upd  (3,4)))]

test_Str = do   assertEqual "\"0\""                             $ serialize $ StrO (Calc (0,0)) "0"
                assertEqual "\"\""                              $ serialize $ StrO (Calc (0,0)) ""

                assertEqual "{_type:UPD,_pos:[1,2],_val:\"0\"}" $ serialize $ StrO (Upd  (1,2)) "0"
                assertEqual "{_type:UPD,_pos:[1,2],_val:\"\"}"  $ serialize $ StrO (Upd  (1,2)) ""

test_Num = do   assertEqual "1.0"                               $ serialize $ NumO (Calc (0,0)) 1
                assertEqual "0.0"                               $ serialize $ NumO (Calc (0,0)) 0

                assertEqual "{_type:UPD,_pos:[1,2],_val:1.0}" $ serialize $ NumO (Upd  (1,2)) 1
                assertEqual "{_type:UPD,_pos:[1,2],_val:0.0}" $ serialize $ NumO (Upd  (1,2)) 0

test_Bool = do  assertEqual "true"                              $ serialize $ BoolO (Calc (0,0)) True
                assertEqual "false"                             $ serialize $ BoolO (Calc (0,0)) False

                assertEqual "{_type:UPD,_pos:[1,2],_val:true}"  $ serialize $ BoolO (Upd  (1,2)) True
                assertEqual "{_type:UPD,_pos:[1,2],_val:false}" $ serialize $ BoolO (Upd  (1,2)) False

test_Null = do  assertEqual "null"                              $ serialize $ NullO (Calc (0,0))
                assertEqual "{_type:UPD,_pos:[1,2],_val:null}"  $ serialize $ NullO (Upd  (1,2))

