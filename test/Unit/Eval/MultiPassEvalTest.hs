{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Unit.Eval.MultiPassEvalTest where

import Data.EvalError
import Test.Framework

import Unit.Eval.MultiPassEvalUtils
import Unit.Parser.MonolithicParserUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

test_MultiDefs = do assertEqual (Left  $ MultipleDefinitions (1,8)  "x")                 $ initProg "x=null;x=true"
                    assertEqual (Left  $ MultipleDefinitions (1,15) "x")                 $ initProg "x=null;y=true;x=null"
test_ValidDefs = do assertEqual (Right $ formTable [])                                   $ initProg ""
                    assertEqual (Right $ formTable [mkForm' (1,1) "x" $ mkNull' (1,3)])  $ initProg "x=null"
                    assertEqual (Right $ formTable [mkForm' (1,1) "x" $ mkNull' (1,3)
                                                   ,mkForm' (1,8) "y" $ mkNull' (1,10)]) $ initProg "x=null;y=null"
                                                                                                    
test_UndefVars = do assertEqual (Left  $ UndefinedVariable (1,3) "y")                    $ derefProg "x=y"
                    assertEqual (Left  $ UndefinedVariable (1,5) "y")                    $ derefProg "x=f(y)"
                    assertEqual (Left  $ UndefinedVariable (1,4) "y")                    $ derefProg "x=[y]"
                    assertEqual (Left  $ UndefinedVariable (1,6) "y")                    $ derefProg "x={x:y}"
test_CycleVars = do assertEqual (Left  $ CycleInDefinitions [((1,1),"x")])               $ derefProg "x=x"
                    assertEqual (Left  $ CycleInDefinitions [((1,1),"x"),((1,8),"y")])   $ derefProg "x=f(y);y=[x]"
                    assertEqual (Left  $ CycleInDefinitions [((1,1),"x"),((1,8),"y")
                                                                        ,((1,14),"z")])  $ derefProg "x=f(y);y=[z];z={a:y}"
test_ValidVars = do assertEqual (Right $ formTable [])                                   $ derefProg ""
                    assertEqual (Right $ formTable [mkForm' (1,1) "x" $ mkNull' (1,7)       
                                                   ,mkForm' (1,5) "y" $ mkNull' (1,7)])  $ derefProg "x=y;y=null"

test_UndefFuncs = do assertEqual (Left $ UndefinedFunction (1,3) "f")                    $ validateProg []    "x=f()"
                     assertEqual (Left $ UndefinedFunction (1,3) "g")                    $ validateProg ["f"] "x=g()"
                     assertEqual (Left $ UndefinedFunction (1,11) "g")                   $ validateProg ["f"] "show=show(g())"
test_NonTopShow = do assertEqual (Left $ NonTopLevelShow   (1,5))                        $ validateProg ["f"] "x=f(show())"
                     assertEqual (Left $ NonTopLevelShow   (1,4))                        $ validateProg []    "x=[show()]"
                     assertEqual (Left $ NonTopLevelShow   (1,6))                        $ validateProg []    "x={a:show()}"
                     assertEqual (Left $ NonTopLevelShow   (1,14))                       $ validateProg []    "show=show({a:show()})"
test_NoShow     = do assertEqual (Left NoShow)                                           $ validateProg []    ""
                     assertEqual (Left NoShow)                                           $ validateProg ["f"] "x=f()"
                     assertEqual (Left NoShow)                                           $ validateProg []    "x=[]"
                     assertEqual (Left NoShow)                                           $ validateProg []    "x={}"
                     assertEqual (Left NoShow)                                           $ validateProg []    "x=show()"
                     assertEqual (Left NoShow)                                           $ validateProg []    "show=null"
test_ValidFuncs = do assertEqual (Right ())                                              $ validateProg []    "show=show()"
                     assertEqual (Right ())                                              $ validateProg []    "show=show(x,y,z);x=null;y=x;z=true"
