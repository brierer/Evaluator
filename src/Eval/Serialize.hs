module Eval.Serialize
( serialize
) where

import Data.Char
import Data.ExpObj
import Data.List

serialize :: ExpObj -> String
serialize e = withPos (getObjPos e) $ specific e

withPos :: ObjPos -> String -> String
withPos (Calc _)    s = s
withPos (Upd (x,y)) s = "{_type:UPD,_pos:" ++ show [x,y] ++ ",_val:"++s++"}"

specific :: ExpObj -> String
specific (TableO _ ess h) = "{_type:TABLE,_data:[" ++ (intercalate "," $ map serializeList ess) ++ "],_head:[" ++ (intercalate "," $ map serialize h) ++ "]}"
specific (PlotO  _ ps  h) = "{_type:PLOT,_data:[" ++ (intercalate "," $ map serializePoint ps) ++ "],_head:{" ++ (intercalate "," $ map serializePair h) ++ "}}" 
specific (ArrO   _ es)    = "[" ++ (intercalate "," $ map serialize     es) ++ "]"
specific (ObjO   _ ps)    = "{" ++ (intercalate "," $ map serializePair ps) ++ "}"
specific (StrO   _ v)     = show v
specific (NumO   _ v)     = show v
specific (BoolO  _ v)     = map toLower $ show v
specific (NullO  _)       = "null"

serializeList :: [ExpObj] -> String
serializeList es = "[" ++ (intercalate "," $ map serialize es) ++ "]"

serializePoint :: (ExpObj,ExpObj) -> String
serializePoint (x,y) = "[" ++ serialize x ++ "," ++ serialize y ++ "]"

serializePair :: (String,ExpObj) -> String
serializePair (x,y) = x ++ ":" ++ serialize y