{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Serialize.SerializeUnit where

import Eval.Serialize
import Marshall.MarshallUtils

import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

test_Table = do assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[]}"                                                              $ serialize $ Right $ mkTableC (1,2) [[mkStrC  (3,4) ""]] []
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[]}"                                                               $ serialize $ Right $ mkTableC (1,2) [[mkNumC  (3,4) 0.0]] []
                assertEqual "{_type:TABLE,_data:[[false]],_head:[]}"                                                             $ serialize $ Right $ mkTableC (1,2) [[mkBoolC (3,4) False]] []
                assertEqual "{_type:TABLE,_data:[[null]],_head:[]}"                                                              $ serialize $ Right $ mkTableC (1,2) [[mkNullC (3,4)]] []

                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[]}"                                  $ serialize $ Right $ mkTableC (1,2) [[mkStrU  (3,4) ""]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[]}"                                   $ serialize $ Right $ mkTableC (1,2) [[mkNumU  (3,4) 0.0]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[]}"                                 $ serialize $ Right $ mkTableC (1,2) [[mkBoolU (3,4) False]] []
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[]}"                                  $ serialize $ Right $ mkTableC (1,2) [[mkNullU (3,4)]] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[]}}"                                  $ serialize $ Right $ mkTableU (1,2) [[mkStrC  (3,4) ""]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}"                                   $ serialize $ Right $ mkTableU (1,2) [[mkNumC  (3,4) 0.0]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[]}}"                                 $ serialize $ Right $ mkTableU (1,2) [[mkBoolC (3,4) False]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[]}}"                                  $ serialize $ Right $ mkTableU (1,2) [[mkNullC (3,4)]] []

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[]}}"      $ serialize $ Right $ mkTableU (1,2) [[mkStrU  (3,4) ""]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[]}}"       $ serialize $ Right $ mkTableU (1,2) [[mkNumU  (3,4) 0.0]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[]}}"     $ serialize $ Right $ mkTableU (1,2) [[mkBoolU (3,4) False]] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[]}}"      $ serialize $ Right $ mkTableU (1,2) [[mkNullU (3,4)]] []

                assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[\"\"]}"                                                          $ serialize $ Right $ mkTableC (1,2) [[mkStrC  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[\"\"]}"                                                           $ serialize $ Right $ mkTableC (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[false]],_head:[\"\"]}"                                                         $ serialize $ Right $ mkTableC (1,2) [[mkBoolC (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[null]],_head:[\"\"]}"                                                          $ serialize $ Right $ mkTableC (1,2) [[mkNullC (3,4)]]       [mkStrC  (5,6) ""]
                                                                                                                                                                                                
                assertEqual "{_type:TABLE,_data:[[\"\"]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                              $ serialize $ Right $ mkTableC (1,2) [[mkStrC  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[0.0]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                               $ serialize $ Right $ mkTableC (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[false]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                             $ serialize $ Right $ mkTableC (1,2) [[mkBoolC (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[null]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"                              $ serialize $ Right $ mkTableC (1,2) [[mkNullC (3,4)]]       [mkStrU  (5,6) ""]
                                                                                                                                                                                                
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[\"\"]}"                              $ serialize $ Right $ mkTableC (1,2) [[mkStrU  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[\"\"]}"                               $ serialize $ Right $ mkTableC (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[\"\"]}"                             $ serialize $ Right $ mkTableC (1,2) [[mkBoolU (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[\"\"]}"                              $ serialize $ Right $ mkTableC (1,2) [[mkNullU (3,4)]]       [mkStrC  (5,6) ""]
                                                                                                                                                                                                
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"  $ serialize $ Right $ mkTableC (1,2) [[mkStrU  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"   $ serialize $ Right $ mkTableC (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}" $ serialize $ Right $ mkTableC (1,2) [[mkBoolU (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}"  $ serialize $ Right $ mkTableC (1,2) [[mkNullU (3,4)]]       [mkStrU  (5,6) ""]
                                                                                                                                                                                                
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[\"\"]}}"                              $ serialize $ Right $ mkTableU (1,2) [[mkStrC  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[\"\"]}}"                               $ serialize $ Right $ mkTableU (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[\"\"]}}"                             $ serialize $ Right $ mkTableU (1,2) [[mkBoolC (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[\"\"]}}"                              $ serialize $ Right $ mkTableU (1,2) [[mkNullC (3,4)]]       [mkStrC  (5,6) ""]
                                                                                                                                                                                                
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[\"\"]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"  $ serialize $ Right $ mkTableU (1,2) [[mkStrC  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[0.0]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"   $ serialize $ Right $ mkTableU (1,2) [[mkNumC  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[false]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}" $ serialize $ Right $ mkTableU (1,2) [[mkBoolC (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[null]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"  $ serialize $ Right $ mkTableU (1,2) [[mkNullC (3,4)]]       [mkStrU  (5,6) ""]
                                                                                                                                                                                                
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[\"\"]}}"  $ serialize $ Right $ mkTableU (1,2) [[mkStrU  (3,4) ""]]    [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[\"\"]}}"   $ serialize $ Right $ mkTableU (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[\"\"]}}" $ serialize $ Right $ mkTableU (1,2) [[mkBoolU (3,4) False]] [mkStrC  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[\"\"]}}"  $ serialize $ Right $ mkTableU (1,2) [[mkNullU (3,4)]]       [mkStrC  (5,6) ""]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:\"\"}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"    $ serialize $ Right $ mkTableU (1,2) [[mkStrU  (3,4) ""]]    [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:0.0}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"     $ serialize $ Right $ mkTableU (1,2) [[mkNumU  (3,4) 0.0]]   [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:false}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"   $ serialize $ Right $ mkTableU (1,2) [[mkBoolU (3,4) False]] [mkStrU  (5,6) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:TABLE,_data:[[{_type:UPD,_pos:[3,4],_val:null}]],_head:[{_type:UPD,_pos:[5,6],_val:\"\"}]}}"    $ serialize $ Right $ mkTableU (1,2) [[mkNullU (3,4)]]       [mkStrU  (5,6) ""]

test_Plot = do  assertEqual "{_type:PLOT,_data:[],_head:{}}"                                                                                                   $ serialize $ Right $ mkPlotC (1,2) [] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{}}}"                                                                       $ serialize $ Right $ mkPlotU (1,2) [] []
                                                                                                                                                                 
                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{}}"                                                                                        $ serialize $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{}}"                                                            $ serialize $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}"                                                            $ serialize $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] []
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}"                                $ serialize $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] []
                                                                                                                                                               
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{}}}"                                                            $ serialize $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{}}}"                                $ serialize $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}}"                                $ serialize $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] []
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{}}}"    $ serialize $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] []
                                                                                                                                                               
                assertEqual "{_type:PLOT,_data:[],_head:{title:\"\"}}"                                                                                         $ serialize $ Right $ mkPlotC (1,2) [] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{title:\"\"}}}"                                                             $ serialize $ Right $ mkPlotU (1,2) [] [("title",mkStrC (7,8) "")]

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{title:\"\"}}"                                                                              $ serialize $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}"                                                  $ serialize $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:\"\"}}"                                                  $ serialize $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}"                      $ serialize $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{title:\"\"}}}"                                                         $ serialize $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:\"\"}}}"                             $ serialize $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}}"                             $ serialize $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:\"\"}}}" $ serialize $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrC (7,8) "")]

                assertEqual "{_type:PLOT,_data:[],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                                                              $ serialize $ Right $ mkPlotC (1,2) [] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                                                                  $ serialize $ Right $ mkPlotU (1,2) [] [("title",mkStrU (7,8) "")]

                assertEqual "{_type:PLOT,_data:[[10.0,11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                                                   $ serialize $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                       $ serialize $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                                                       $ serialize $ Right $ mkPlotC (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}"                           $ serialize $ Right $ mkPlotC (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]

                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                                                         $ serialize $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},11.0]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                             $ serialize $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumC (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[10.0,{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}"                             $ serialize $ Right $ mkPlotU (1,2) [(mkNumC (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{_type:PLOT,_data:[[{_type:UPD,_pos:[3,4],_val:10.0},{_type:UPD,_pos:[5,6],_val:11.0}]],_head:{title:{_type:UPD,_pos:[7,8],_val:\"\"}}}}" $ serialize $ Right $ mkPlotU (1,2) [(mkNumU (3,4) 10.0,mkNumU (5,6) 11.0)] [("title",mkStrU (7,8) "")]

test_Arr = do   assertEqual "[{_type:TABLE,_data:[[0.0]],_head:[]}]"                            $ serialize $ Right $ mkArrC (1,2) [mkTableC (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "[{_type:PLOT,_data:[],_head:{}}]"                                  $ serialize $ Right $ mkArrC (1,2) [mkPlotC  (3,4) [] []]
                assertEqual "[[]]"                                                              $ serialize $ Right $ mkArrC (1,2) [mkArrC   (3,4) []]
                assertEqual "[{}]"                                                              $ serialize $ Right $ mkArrC (1,2) [mkObjC   (3,4) []]
                assertEqual "[\"\"]"                                                            $ serialize $ Right $ mkArrC (1,2) [mkStrC   (3,4) ""]
                assertEqual "[0.0]"                                                             $ serialize $ Right $ mkArrC (1,2) [mkNumC   (3,4) 0.0]
                assertEqual "[false]"                                                           $ serialize $ Right $ mkArrC (1,2) [mkBoolC  (3,4) False]
                assertEqual "[null]"                                                            $ serialize $ Right $ mkArrC (1,2) [mkNullC  (3,4)]
                                                                                                                                        
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}]" $ serialize $ Right $ mkArrC (1,2) [mkTableU (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}]"       $ serialize $ Right $ mkArrC (1,2) [mkPlotU  (3,4) [] []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:[]}]"                                   $ serialize $ Right $ mkArrC (1,2) [mkArrU   (3,4) []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:{}}]"                                   $ serialize $ Right $ mkArrC (1,2) [mkObjU   (3,4) []]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:\"\"}]"                                 $ serialize $ Right $ mkArrC (1,2) [mkStrU   (3,4) ""]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:0.0}]"                                  $ serialize $ Right $ mkArrC (1,2) [mkNumU   (3,4) 0.0]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:false}]"                                $ serialize $ Right $ mkArrC (1,2) [mkBoolU  (3,4) False]
                assertEqual "[{_type:UPD,_pos:[3,4],_val:null}]"                                 $ serialize $ Right $ mkArrC (1,2) [mkNullU  (3,4)]
                                                                                                                                        
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:TABLE,_data:[[0.0]],_head:[]}]}" $ serialize $ Right $ mkArrU (1,2) [mkTableC (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:PLOT,_data:[],_head:{}}]}"       $ serialize $ Right $ mkArrU (1,2) [mkPlotC  (3,4) [] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[[]]}"                                   $ serialize $ Right $ mkArrU (1,2) [mkArrC   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{}]}"                                   $ serialize $ Right $ mkArrU (1,2) [mkObjC   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[\"\"]}"                                 $ serialize $ Right $ mkArrU (1,2) [mkStrC   (3,4) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[0.0]}"                                  $ serialize $ Right $ mkArrU (1,2) [mkNumC   (3,4) 0.0]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[false]}"                                $ serialize $ Right $ mkArrU (1,2) [mkBoolC  (3,4) False]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[null]}"                                 $ serialize $ Right $ mkArrU (1,2) [mkNullC  (3,4)]
                                                                                                                                        
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}]}" $ serialize $ Right $ mkArrU (1,2) [mkTableU (3,4) [[mkNumC  (0,0) 0.0]] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}]}"       $ serialize $ Right $ mkArrU (1,2) [mkPlotU  (3,4) [] []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:[]}]}"                                   $ serialize $ Right $ mkArrU (1,2) [mkArrU   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:{}}]}"                                   $ serialize $ Right $ mkArrU (1,2) [mkObjU   (3,4) []]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:\"\"}]}"                                 $ serialize $ Right $ mkArrU (1,2) [mkStrU   (3,4) ""]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:0.0}]}"                                  $ serialize $ Right $ mkArrU (1,2) [mkNumU   (3,4) 0.0]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:false}]}"                                $ serialize $ Right $ mkArrU (1,2) [mkBoolU  (3,4) False]
                assertEqual "{_type:UPD,_pos:[1,2],_val:[{_type:UPD,_pos:[3,4],_val:null}]}"                                 $ serialize $ Right $ mkArrU (1,2) [mkNullU  (3,4)]
                
test_Obj = do   assertEqual "{x:{_type:TABLE,_data:[[0.0]],_head:[]}}"                           $ serialize $ Right $ mkObjC (1,2) [("x",mkTableC (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{x:{_type:PLOT,_data:[],_head:{}}}"                                 $ serialize $ Right $ mkObjC (1,2) [("x",mkPlotC  (3,4) [] [])]
                assertEqual "{x:[]}"                                                             $ serialize $ Right $ mkObjC (1,2) [("x",mkArrC   (3,4) [])]
                assertEqual "{x:{}}"                                                             $ serialize $ Right $ mkObjC (1,2) [("x",mkObjC   (3,4) [])]
                assertEqual "{x:\"\"}"                                                           $ serialize $ Right $ mkObjC (1,2) [("x",mkStrC   (3,4) "")]
                assertEqual "{x:0.0}"                                                            $ serialize $ Right $ mkObjC (1,2) [("x",mkNumC   (3,4) 0)]
                assertEqual "{x:false}"                                                          $ serialize $ Right $ mkObjC (1,2) [("x",mkBoolC  (3,4) False)]
                assertEqual "{x:null}"                                                           $ serialize $ Right $ mkObjC (1,2) [("x",mkNullC  (3,4))]
                                                                                                                                              
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}}" $ serialize $ Right $ mkObjC (1,2) [("x",mkTableU (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}}"       $ serialize $ Right $ mkObjC (1,2) [("x",mkPlotU  (3,4) [] [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:[]}}"                                   $ serialize $ Right $ mkObjC (1,2) [("x",mkArrU   (3,4) [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:{}}}"                                   $ serialize $ Right $ mkObjC (1,2) [("x",mkObjU   (3,4) [])]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:\"\"}}"                                 $ serialize $ Right $ mkObjC (1,2) [("x",mkStrU   (3,4) "")]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:0.0}}"                                  $ serialize $ Right $ mkObjC (1,2) [("x",mkNumU   (3,4) 0)]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:false}}"                                $ serialize $ Right $ mkObjC (1,2) [("x",mkBoolU  (3,4) False)]
                assertEqual "{x:{_type:UPD,_pos:[3,4],_val:null}}"                                 $ serialize $ Right $ mkObjC (1,2) [("x",mkNullU  (3,4))]
                                                                                                                                              
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:TABLE,_data:[[0.0]],_head:[]}}}" $ serialize $ Right $ mkObjU (1,2) [("x",mkTableC (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:PLOT,_data:[],_head:{}}}}"       $ serialize $ Right $ mkObjU (1,2) [("x",mkPlotC  (3,4) [] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:[]}}"                                   $ serialize $ Right $ mkObjU (1,2) [("x",mkArrC   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{}}}"                                   $ serialize $ Right $ mkObjU (1,2) [("x",mkObjC   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:\"\"}}"                                 $ serialize $ Right $ mkObjU (1,2) [("x",mkStrC   (3,4) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:0.0}}"                                  $ serialize $ Right $ mkObjU (1,2) [("x",mkNumC   (3,4) 0)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:false}}"                                $ serialize $ Right $ mkObjU (1,2) [("x",mkBoolC  (3,4) False)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:null}}"                                 $ serialize $ Right $ mkObjU (1,2) [("x",mkNullC  (3,4))]
                                                                                                                                              
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:{_type:TABLE,_data:[[0.0]],_head:[]}}}}" $ serialize $ Right $ mkObjU (1,2) [("x",mkTableU (3,4) [[mkNumC  (0,0) 0.0]] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:{_type:PLOT,_data:[],_head:{}}}}}"       $ serialize $ Right $ mkObjU (1,2) [("x",mkPlotU  (3,4) [] [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:[]}}}"                                   $ serialize $ Right $ mkObjU (1,2) [("x",mkArrU   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:{}}}}"                                   $ serialize $ Right $ mkObjU (1,2) [("x",mkObjU   (3,4) [])]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:\"\"}}}"                                 $ serialize $ Right $ mkObjU (1,2) [("x",mkStrU   (3,4) "")]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:0.0}}}"                                  $ serialize $ Right $ mkObjU (1,2) [("x",mkNumU   (3,4) 0)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:false}}}"                                $ serialize $ Right $ mkObjU (1,2) [("x",mkBoolU  (3,4) False)]
                assertEqual "{_type:UPD,_pos:[1,2],_val:{x:{_type:UPD,_pos:[3,4],_val:null}}}"                                 $ serialize $ Right $ mkObjU (1,2) [("x",mkNullU  (3,4))]

test_Str = do   assertEqual "\"0\""                             $ serialize $ Right $ mkStrC (0,0) "0"
                assertEqual "\"\""                              $ serialize $ Right $ mkStrC (0,0) ""

                assertEqual "{_type:UPD,_pos:[1,2],_val:\"0\"}" $ serialize $ Right $ mkStrU (1,2) "0"
                assertEqual "{_type:UPD,_pos:[1,2],_val:\"\"}"  $ serialize $ Right $ mkStrU (1,2) ""

test_Num = do   assertEqual "1.0"                               $ serialize $ Right $ mkNumC (0,0) 1
                assertEqual "0.0"                               $ serialize $ Right $ mkNumC (0,0) 0

                assertEqual "{_type:UPD,_pos:[1,2],_val:1.0}"   $ serialize $ Right $ mkNumU (1,2) 1
                assertEqual "{_type:UPD,_pos:[1,2],_val:0.0}"   $ serialize $ Right $ mkNumU (1,2) 0

test_Bool = do  assertEqual "true"                              $ serialize $ Right $ mkBoolC (0,0) True
                assertEqual "false"                             $ serialize $ Right $ mkBoolC (0,0) False

                assertEqual "{_type:UPD,_pos:[1,2],_val:true}"  $ serialize $ Right $ mkBoolU (1,2) True
                assertEqual "{_type:UPD,_pos:[1,2],_val:false}" $ serialize $ Right $ mkBoolU (1,2) False

test_Null = do  assertEqual "null"                              $ serialize $ Right $ mkNullC (0,0)
                assertEqual "{_type:UPD,_pos:[1,2],_val:null}"  $ serialize $ Right $ mkNullU (1,2)

