module Eval.Serialize
( serialize,
  serializeErr,
  serializeExp
) where

import Data.Char
import Data.Eval
import Data.EvalError
import Data.ExpObj
import Data.List
import Data.Type
import Data.ExpToken

serialize :: Eval ExpObj -> String
serialize (Right e) = serializeExp e
serialize (Left e)  = serializeErr e

serializeEvalTable :: Eval Table -> String
serializeEvalTable (Right t) = serializeTable t
serializeEvalTable (Left e) = serializeErr e

serializeTable :: Table -> String
serializeTable t = show t

serializeExp :: ExpObj -> String
serializeExp e = withPos (getObjPos e) $ specific e

withPos :: ObjPos -> String -> String
withPos (Calc{})    s = s
withPos (Upd (x,y)) s = customObj "UPD" [("_pos",show [x,y]),("_val",s)]

specific :: ExpObj -> String
specific (PlotO  _ ps  h) = customObj "PLOT"  [("_data", mapArr ps  serializePoint),("_head", between "{" "}" serializePair h)]
specific (ArrO   _ es)    = serializeArr es
specific (ObjO   _ ps)    = serializeObj ps
specific (StrO   _ v)     = show v
specific (NumO   _ v)     = show v
specific (BoolO  _ v)     = map toLower $ show v
specific (NullO  _)       = "null"

customObj :: String -> [(String,String)] -> String
customObj t ps = between "{" "}" (\(x,y) -> show x ++ ":" ++ y) $ ("_type",show t):ps

serializeArr :: [ExpObj] -> String
serializeArr = between "[" "]" serializeExp

serializeObj :: [(String,ExpObj)] -> String
serializeObj = between "{sdf" "}" serializePair

serializePoint :: (ExpObj,ExpObj) -> String
serializePoint (x,y) = "[" ++ serializeExp x ++ "," ++ serializeExp y ++ "]"

serializePair :: (String,ExpObj) -> String
serializePair (x,y) = show x ++ ":" ++ serializeExp y

between :: String -> String -> (a -> String) -> [a] -> String
between a b f xs = a ++ intercalate "," (map f xs) ++ b

mapArr :: [a] -> (a -> String) -> String
mapArr xs f = between "[" "]" f xs

serializeErr :: EvalError -> String
serializeErr (InvalidParse              p ss)      = errorObj  "PARSE"               p [("_data",mapArr ss show)]
serializeErr (MultipleDefinitions       p s)       = errorObj  "MULT_DEFS"           p [("_data",show s)]
serializeErr (UndefinedVariable         p s)       = errorObj  "UNDEF_VAR"           p [("_data",show s)]
serializeErr (CycleInDefinitions        ps)        = customObj "ERROR"                 [("_errType",show "CYCLE"),("_data",mapArr ps $ \((x,y),s) -> "[" ++ show [x,y] ++ "," ++ show s ++ "]")]
serializeErr (UndefinedFunction         p s)       = errorObj  "UNDEF_FUNC"          p [("_data",show s)]
serializeErr (NonTopLevelShow           p)         = errorObj  "NON_TOP_SHOW"        p []
serializeErr  NoShow                               = customObj "ERROR"                 [("_errType",show "NO_SHOW")]
serializeErr (ArgCountMismatch          p s e a)   = errorObj  "ARG_COUNT"           p [("_func",show s),("_exp",show e),("_act",show a)]
serializeErr (TypeMismatch              t)         = customObj "ERROR"                 [("_errType",show "TYPE_MISMATCH"),("_data",typeError t)]
serializeErr (IllegalEmpty              p)         = errorObj  "ILLEGAL_EMPTY"       p []
serializeErr (TableColumnLengthMismatch p e a)     = errorObj  "TABLE_COL_LEN"       p [("_exp",show e),("_act",show a)]
serializeErr (TableHeaderLengthMismatch p e a)     = errorObj  "TABLE_HEAD_LEN"      p [("_exp",show e),("_act",show a)]
serializeErr (IllegalTakeTableLength    p e a)     = errorObj  "TABLE_TAKE_LEN"      p [("_exp",show e),("_act",show a)]
serializeErr (IndexOutOfBounds          p mi ma a) = errorObj  "INDEX_OUT_OF_BOUNDS" p [("_min",show mi),("_max",show ma),("_act",show a)]

errorObj :: String -> (Int, Int) -> [(String, String)] -> String
errorObj t (x,y) ps = customObj "ERROR" $ [("_errType",show t),("_pos",show [x,y])] ++ ps

typeError :: TMTree -> String
typeError (TMNode ts)        = customObj "TM_NODE" [("_data",mapArr ts typeError)]
typeError (TMLeaf (x,y) e a) = customObj "TM_LEAF" [("_pos",show [x,y]),("_exp",toType e),("_act",toType a)]

toType :: TypeHead -> String
toType (NodeOr ts) = customObj "OR_TYPE" [("_data",mapArr ts toType)]
toType t = show $ simpleType t

simpleType :: TypeHead -> String
simpleType LeafTable = "Table"
simpleType LeafPlot  = "Plot"
simpleType NodeArr   = "Array"
simpleType NodeObj   = "Object"
simpleType LeafStr   = "String"
simpleType LeafNum   = "Number"
simpleType LeafBool  = "Boolean"
simpleType LeafNull  = "Null"
simpleType x         = error $ "Serialize::simpleType [Unexpected pattern ["++show x++"]]"

