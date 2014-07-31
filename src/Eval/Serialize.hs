module Eval.Serialize
( serialize
) where

import Data.Char
import Data.ExpObj
import Data.List

serialize :: ExpObj -> String
serialize e = withPos (getObjPos e) $ specific e

withPos :: ObjPos -> String -> String
withPos (Calc{})    s = s
withPos (Upd (x,y)) s = "{_type:UPD,_pos:" ++ show [x,y] ++ ",_val:"++s++"}"

specific :: ExpObj -> String
specific (TableO _ ess h) = "{_type:TABLE,_data:" ++ between "[" "]" serializeList ess ++ ",_head:" ++ between "[" "]" serialize h ++ "}"
specific (PlotO  _ ps  h) = "{_type:PLOT,_data:"  ++ between "[" "]" serializePoint ps ++ ",_head:" ++ between "{" "}" serializePair h ++ "}" 
specific (ArrO   _ es)    = between "[" "]" serialize es
specific (ObjO   _ ps)    = between "{" "}" serializePair ps 
specific (StrO   _ v)     = show v
specific (NumO   _ v)     = show v
specific (BoolO  _ v)     = map toLower $ show v
specific (NullO  _)       = "null"

serializeList :: [ExpObj] -> String
serializeList = between "[" "]" serialize

serializePoint :: (ExpObj,ExpObj) -> String
serializePoint (x,y) = "[" ++ serialize x ++ "," ++ serialize y ++ "]"

serializePair :: (String,ExpObj) -> String
serializePair (x,y) = x ++ ":" ++ serialize y

between :: String -> String -> (a -> String) -> [a] -> String
between a b f xs = a ++ intercalate "," (map f xs) ++ b