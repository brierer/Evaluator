module Eval.Serialize
( serialize
) where

import Data.Char
import Data.Eval
import Data.ExpObj
import Data.List

serialize :: Eval ExpObj -> String
serialize (Right e) = serializeExp e
serialize (Left _)  = error "Eval.Serialize::serialize::Left [Not Implemented]"
 
serializeExp :: ExpObj -> String
serializeExp e = withPos (getObjPos e) $ specific e

withPos :: ObjPos -> String -> String
withPos (Calc{})    s = s
withPos (Upd (x,y)) s = "{_type:UPD,_pos:" ++ show [x,y] ++ ",_val:"++s++"}"

specific :: ExpObj -> String
specific (TableO _ ess h) = "{_type:TABLE,_data:" ++ between "[" "]" serializeList ess ++ ",_head:" ++ between "[" "]" serializeExp h ++ "}"
specific (PlotO  _ ps  h) = "{_type:PLOT,_data:"  ++ between "[" "]" serializePoint ps ++ ",_head:" ++ between "{" "}" serializePair h ++ "}" 
specific (ArrO   _ es)    = between "[" "]" serializeExp es
specific (ObjO   _ ps)    = between "{" "}" serializePair ps 
specific (StrO   _ v)     = show v
specific (NumO   _ v)     = show v
specific (BoolO  _ v)     = map toLower $ show v
specific (NullO  _)       = "null"

serializeList :: [ExpObj] -> String
serializeList = between "[" "]" serializeExp

serializePoint :: (ExpObj,ExpObj) -> String
serializePoint (x,y) = "[" ++ serializeExp x ++ "," ++ serializeExp y ++ "]"

serializePair :: (String,ExpObj) -> String
serializePair (x,y) = x ++ ":" ++ serializeExp y

between :: String -> String -> (a -> String) -> [a] -> String
between a b f xs = a ++ intercalate "," (map f xs) ++ b