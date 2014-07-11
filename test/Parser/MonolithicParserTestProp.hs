{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Parser.MonolithicParserTestProp where

import Data.ExpToken
import Parser.Monolithic
import Parser.MonolithicParserTestUtils
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Program
prop_Prog (ProgTA prog)    =                prog .= testCase progT prog
prop_Form (FormTA form)    =                form .= testCase formT form
prop_Pair (PairTA pair)    =                pair .= testCase pairT pair
prop_Id   (IdTA i)         =                i    .= testCase idT i
prop_Exp  (ExpTA e)        =                e    .= testCase expT e
                                            
-- Composite expressions                    
prop_Func  (FuncTA func)   =                func .= testCase funcT func
prop_Array (ArrTA  arr)    =                arr  .= testCase arrT  arr
prop_Obj   (ObjTA  obj)    =                obj  .= testCase objT  obj
                                            
-- Atomic expressions                       
prop_Var    (VarTA var)    =                var .= testCase varT var
prop_String (StrTA str)    =                str .= testCase strT str

prop_Num_int (NumTA t int) = (t == Int) ==> int .= testCase numT int
prop_Num_flt (NumTA t flt) = (t == Flt) ==> flt .= testCase numT flt
prop_Num_exp (NumTA t flt) = (t == Exp) ==> flt .= testCase numT flt

prop_Bool (BoolTA b)       =                b   .= testCase boolT b
prop_Null (NullTA n)       =                n   .= testCase nullT n

{-| Utils -}
testCase p = unsafeParse p.unparse
a .= b = noPos a == noPos b

class    NoPos a         where noPos :: a -> a
instance NoPos ProgToken where noPos (ProgT _ fs) = ProgT p0 $ map noPos fs
instance NoPos FormToken where noPos (FormT i e)  = FormT (noPos i) (noPos e)
instance NoPos PairToken where noPos (PairT i e)  = PairT (noPos i) (noPos e)
instance NoPos IdToken   where noPos (IdT _ w i)  = IdT   p0 w i
instance NoPos ExpToken  where
  noPos(FuncT   w  i es) = FuncT    w (noPos i) $ map noPos es
  noPos(ArrT  _ w    es) = ArrT  p0 w           $ map noPos es
  noPos(ObjT  _ w    ps) = ObjT  p0 w           $ map noPos ps
  noPos(VarT       i   ) = VarT       (noPos i)
  noPos(StrT  _ w   v  ) = StrT  p0 w   v
  noPos(NumT  _ w s v  ) = NumT  p0 w s v
  noPos(BoolT _ w   v  ) = BoolT p0 w   v
  noPos(NullT _ w      ) = NullT p0 w



