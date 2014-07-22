{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Marshall.MarshallUnit where

import Prelude hiding (any)

import Data.EvalError
import Data.ExpObj
import Data.Type
import Eval.MatchType
import Test.Framework

import Marshall.MarshallUnitUtils
import Marshall.MarshallUtils
import Parser.ParserUtils

test_NbArgs = do     assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 0
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 0 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 0
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 0) $ runFuncWith "f()"    $ nbArgEntry "f" 1
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 1 2) $ runFuncWith "f(0,1)" $ nbArgEntry "f" 1
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 0) $ runFuncWith "f()"    $ nbArgEntry "f" 2
                     assertEqual (Left $ ArgCountMismatch (1,1) "f" 2 1) $ runFuncWith "f(0)"   $ nbArgEntry "f" 2

test_ArgTypes = do   assertEqual (Left $ TypeMismatch (1,3) LeafTable LeafNull)  $ runFuncWith "f(null)"      $ singleTypeEntry "f" Table
                     assertEqual (Left $ TypeMismatch (1,3) LeafPlot  LeafTable) $ runFuncWith "f(mkTable())" $ singleTypeEntry "f" Plot ++ noArgFunc "mkTable" (\p -> TableO p [] [])
                     assertEqual (Left $ TypeMismatch (1,3) NodeArr   LeafPlot)  $ runFuncWith "f(mkPlot())"  $ singleTypeEntry "f" arr  ++ noArgFunc "mkPlot"  (\p -> PlotO  p [] [])
                     assertEqual (Left $ TypeMismatch (1,3) NodeObj   NodeArr)   $ runFuncWith "f([])"        $ singleTypeEntry "f" obj
                     assertEqual (Left $ TypeMismatch (1,3) LeafStr   NodeObj)   $ runFuncWith "f({})"        $ singleTypeEntry "f" Str
                     assertEqual (Left $ TypeMismatch (1,3) LeafNum   LeafStr)   $ runFuncWith "f(\"\")"      $ singleTypeEntry "f" Num
                     assertEqual (Left $ TypeMismatch (1,3) LeafBool  LeafNum)   $ runFuncWith "f(0)"         $ singleTypeEntry "f" Bool
                     assertEqual (Left $ TypeMismatch (1,3) LeafNull  LeafBool)  $ runFuncWith "f(false)"     $ singleTypeEntry "f" Null
            
                     assertEqual (Left $ TypeMismatch (1,13) LeafTable LeafPlot)  $ runFuncWith "f(mkTable(),mkPlot())" $ doubleTypeEntry "f" Table  ++ noArgFunc' "mkTable" (TableO p0 [] []) ++ noArgFunc "mkPlot"  (\p -> PlotO p [] [])
                     assertEqual (Left $ TypeMismatch (1,12) LeafPlot  NodeArr)   $ runFuncWith "f(mkPlot(),[])"        $ doubleTypeEntry "f" Plot   ++ noArgFunc' "mkPlot"  (PlotO  p0 [] [])
                     assertEqual (Left $ TypeMismatch (1,6)  NodeArr   NodeObj)   $ runFuncWith "f([],{})"              $ doubleTypeEntry "f" arr  
                     assertEqual (Left $ TypeMismatch (1,6)  NodeObj   LeafStr)   $ runFuncWith "f({},\"\")"            $ doubleTypeEntry "f" obj  
                     assertEqual (Left $ TypeMismatch (1,6)  LeafStr   LeafNum)   $ runFuncWith "f(\"\",0)"             $ doubleTypeEntry "f" Str  
                     assertEqual (Left $ TypeMismatch (1,5)  LeafNum   LeafBool)  $ runFuncWith "f(0,false)"            $ doubleTypeEntry "f" Num 
                     assertEqual (Left $ TypeMismatch (1,9)  LeafBool  LeafNull)  $ runFuncWith "f(false,null)"         $ doubleTypeEntry "f" Bool 
                     assertEqual (Left $ TypeMismatch (1,8)  LeafNull  LeafTable) $ runFuncWith "f(null,mkTable())"     $ doubleTypeEntry "f" Null  ++ noArgFunc "mkTable" (\p -> TableO p [] [])
            
                     assertEqual (Left $ TypeMismatch (1,10)  LeafBool                     LeafNull) $ runFuncWith "f([false,null])" $ singleTypeEntry "f" $ ArrOf Bool
                     assertEqual (Left $ TypeMismatch (1,10)  LeafNum                      LeafStr)  $ runFuncWith "f({x:0,y:\"\"})" $ singleTypeEntry "f" $ ObjOf Num
                     assertEqual (Left $ TypeMismatch (1,3)  (NodeOr [LeafBool, LeafNull]) LeafNum)  $ runFuncWith "f(0)"            $ singleTypeEntry "f" $ Or [Bool,Null]
                     
                     assertEqual mockSuccess $ runFuncWith "f(mkTable())" $ ("f", [Table], mockSuccessFunc) : noArgFunc' "mkTable" (TableO p0 [] [])
                     assertEqual mockSuccess $ runFuncWith "f(mkPlot())"  $ ("f", [Plot],  mockSuccessFunc) : noArgFunc' "mkPlot"  (PlotO  p0 [] [])
                     assertEqual mockSuccess $ runFuncWith "f([])"         [("f", [arr],   mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f({})"         [("f", [obj],   mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f(\"\")"       [("f", [Str],   mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f(0)"          [("f", [Num],   mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f(false)"      [("f", [Bool],  mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f()"           [("f", [],      mockSuccessFunc)]
                                 
                     assertEqual mockSuccess $ runFuncWith "f([true,false])" [("f", [ArrOf Bool],     mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f({x:1,y:0})"    [("f", [ObjOf Num],      mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f(false)"        [("f", [Or [Bool,Null]], mockSuccessFunc)]
                     assertEqual mockSuccess $ runFuncWith "f(null)"         [("f", [Or [Bool,Null]], mockSuccessFunc)]
                     
                     -- When args are themselves function calls
                     assertEqual (Left $ TypeMismatch (1,3) NodeObj  NodeArr)   $ runFuncWith "f(mkArr())"  $ singleTypeEntry "f" obj  ++ noArgFunc "mkArr"  (`ArrO` [])
                     assertEqual (Left $ TypeMismatch (1,3) LeafStr  NodeObj)   $ runFuncWith "f(mkObj())"  $ singleTypeEntry "f" Str  ++ noArgFunc "mkObj"  (`ObjO` [])
                     assertEqual (Left $ TypeMismatch (1,3) LeafNum  LeafStr)   $ runFuncWith "f(mkStr())"  $ singleTypeEntry "f" Num  ++ noArgFunc "mkStr"  (`StrO` "")
                     assertEqual (Left $ TypeMismatch (1,3) LeafBool LeafNum)   $ runFuncWith "f(mkNum())"  $ singleTypeEntry "f" Bool ++ noArgFunc "mkNum"  (`NumO` 0)
                     assertEqual (Left $ TypeMismatch (1,3) LeafNull LeafBool)  $ runFuncWith "f(mkBool())" $ singleTypeEntry "f" Null ++ noArgFunc "mkBool" (`BoolO` False)
                     assertEqual (Left $ TypeMismatch (1,3) NodeArr  LeafNull)  $ runFuncWith "f(mkNull())" $ singleTypeEntry "f" arr  ++ noArgFunc "mkNull" NullO
            
                     assertEqual (Left $ TypeMismatch (1,12) LeafPlot NodeArr)  $ runFuncWith "f(mkPlot(),mkArr())"   $ doubleTypeEntry "f" Plot ++ noArgFunc' "mkPlot" (PlotO  p0 [] []) ++ noArgFunc "mkArr"  (`ArrO` [])
                     assertEqual (Left $ TypeMismatch (1,11) NodeArr  NodeObj)  $ runFuncWith "f(mkArr(),mkObj())"    $ doubleTypeEntry "f" arr  ++ noArgFunc  "mkArr"  (`ArrO` [])       ++ noArgFunc "mkObj"  (`ObjO` [])
                     assertEqual (Left $ TypeMismatch (1,11) NodeObj  LeafStr)  $ runFuncWith "f(mkObj(),mkStr())"    $ doubleTypeEntry "f" obj  ++ noArgFunc  "mkObj"  (`ObjO` [])       ++ noArgFunc "mkStr"  (`StrO` "")
                     assertEqual (Left $ TypeMismatch (1,11) LeafStr  LeafNum)  $ runFuncWith "f(mkStr(),mkNum())"    $ doubleTypeEntry "f" Str  ++ noArgFunc  "mkStr"  (`StrO` "")       ++ noArgFunc "mkNum"  (`NumO` 0)
                     assertEqual (Left $ TypeMismatch (1,11) LeafNum  LeafBool) $ runFuncWith "f(mkNum(),mkBool())"   $ doubleTypeEntry "f" Num  ++ noArgFunc  "mkNum"  (`NumO` 0)        ++ noArgFunc "mkBool" (`BoolO` False)
                     assertEqual (Left $ TypeMismatch (1,12) LeafBool LeafNull) $ runFuncWith "f(mkBool(),mkNull())"  $ doubleTypeEntry "f" Bool ++ noArgFunc  "mkBool" (`BoolO` False)   ++ noArgFunc "mkNull"   NullO
                     assertEqual (Left $ TypeMismatch (1,12) LeafNull LeafPlot) $ runFuncWith "f(mkNull(),mkPlot())"  $ doubleTypeEntry "f" Null ++ noArgFunc  "mkNull"   NullO           ++ noArgFunc "mkPlot" (\p -> PlotO p [] [])
            
                     assertEqual (Left $ TypeMismatch (1,13)  LeafBool                     LeafNull) $ runFuncWith "f([mkBool(),mkNull()])"   $ singleTypeEntry "f" (ArrOf Bool)     ++ noArgFunc "mkBool" (`BoolO` False)   ++ noArgFunc "mkNull"    NullO
                     assertEqual (Left $ TypeMismatch (1,16)  LeafNum                      LeafStr)  $ runFuncWith "f({x:mkNum(),y:mkStr()})" $ singleTypeEntry "f" (ObjOf Num)      ++ noArgFunc "mkStr"  (`StrO` "")       ++ noArgFunc "mkNum"   (`NumO` 0)
                     assertEqual (Left $ TypeMismatch (1,3)  (NodeOr [LeafBool, LeafNull]) LeafNum)  $ runFuncWith "f(mkNum())"               $ singleTypeEntry "f" (Or [Bool,Null]) ++ noArgFunc "mkNum"  (`NumO` 0)
                     
                     assertEqual mockSuccess $ runFuncWith "f(mkArr())"  $ ("f", [arr],   mockSuccessFunc) : noArgFunc "mkArr"  (`ArrO` [])
                     assertEqual mockSuccess $ runFuncWith "f(mkObj())"  $ ("f", [obj],   mockSuccessFunc) : noArgFunc "mkObj"  (`ObjO` [])
                     assertEqual mockSuccess $ runFuncWith "f(mkStr())"  $ ("f", [Str],   mockSuccessFunc) : noArgFunc "mkStr"  (`StrO` "")
                     assertEqual mockSuccess $ runFuncWith "f(mkNum())"  $ ("f", [Num],   mockSuccessFunc) : noArgFunc "mkNum"  (`NumO` 0)
                     assertEqual mockSuccess $ runFuncWith "f(mkBool())" $ ("f", [Bool],  mockSuccessFunc) : noArgFunc "mkBool" (`BoolO` False)
                                 
                     assertEqual mockSuccess $ runFuncWith "f([mkTrue(),mkFalse()])"   $ ("f", [ArrOf Bool],     mockSuccessFunc) : noArgFunc "mkTrue" (`BoolO` True) ++ noArgFunc "mkFalse" (`BoolO` False)
                     assertEqual mockSuccess $ runFuncWith "f({x:mkOne(),y:mkZero()})" $ ("f", [ObjOf Num],      mockSuccessFunc) : noArgFunc "mkOne"  (`NumO` 1)     ++ noArgFunc "mkZero"  (`NumO` 0)
                     assertEqual mockSuccess $ runFuncWith "f(mkBool())"               $ ("f", [Or [Bool,Null]], mockSuccessFunc) : noArgFunc "mkBool" (`BoolO` False)
                     assertEqual mockSuccess $ runFuncWith "f(mkNull())"               $ ("f", [Or [Bool,Null]], mockSuccessFunc) : noArgFunc "mkNull" NullO


                    