{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-orphans #-}
module Parser.MonolithicTest where

import Test.Framework
import Numeric
import TestUtils

import Parser.Monolithic
import Data.Token

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant do" #-}

-- Program
prop_prog fs                 = parseCase (ProgT fs)              progT  (parseable (ProgT fs))
prop_form (IdA s) e          = parseCase (FormT s e)             formT  (parseable (FormT s e))
                                                                      
-- Composite expressions                                              
prop_func (IdA s) es         = parseCase (FuncT s es)            funcT  (parseable (FuncT s es))
prop_array es                = parseCase (ArrayT es)             arrayT (parseable (ArrayT es))
prop_obj ps                  = parseCase (ObjT (map unPair ps))  objT   (parseable (ObjT (map unPair ps)))
                                                                      
prop_exp e                   = parseCase e                       expT   (parseable (e :: ExpToken))
prop_pair p@(Pair (IdA s) e) = parseCase (s,e)                   pairT  (parseable p)
                                                                      
-- Atomic expressions                                                 
prop_var    (IdA s)          = parseCase (VarT s)                varT   (parseable (VarT s))
prop_string (StrTA s)        = parseCase (StrT s)                strT   (parseable (StrT s))
                                                                      
prop_number_int i            = parseCase (NumT (fromIntegral i)) numT   [show (i :: Integer)]
prop_number_flt f            = parseCase (NumT f)                numT   [show (f :: Double)]
prop_number_exp f            = parseCase (NumT f)                numT   [showEFloat Nothing f ""]
                                                                     
prop_bool b                  = parseCase (BoolT b)               boolT  (parseable (BoolT b))
prop_null                    = parseCase NullT                   nullT  (parseable NullT)




