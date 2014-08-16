module Serialize.SerializeUtils where

import Text.JSON
import Data.List
import Data.List.Split
import Eval.Serialize

serializeValid x = let r = serialize x in case decode r :: Result JSValue of Error s -> r ++ " [" ++ s ++ "]"; _ -> unString r

unString r = foldl removeQuotes r keywords
removeQuotes s v = intercalate v $ splitOn ("\"" ++ v ++ "\"") s

keywords = [ "null","false","true"
           , "_type","_pos","_val","_data","_head","_x","_errType","_func","_exp","_act","_min","_max"
           , "UPD","TABLE","PLOT","OR_TYPE"
           , "ERROR","PARSE","MULT_DEFS","UNDEF_VAR","CYCLE","UNDEF_FUNC","NON_TOP_SHOW","NO_SHOW"
           , "ARG_COUNT","TYPE_MISMATCH","TM_NODE","TM_LEAF","ILLEGAL_EMPTY","TABLE_COL_LEN","TABLE_HEAD_LEN","TABLE_TAKE_LEN","INDEX_OUT_OF_BOUNDS"
           , "title"
           ]