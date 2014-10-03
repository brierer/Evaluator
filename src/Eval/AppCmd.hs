
module Eval.AppCmd (
	appCmd,
	) where

import qualified Data.Map as M
import Data.Maybe

import Data.EvalCmd
import Data.ExpToken
import Data.EvalCmd
import Data.Eval
import Eval.Parser
import Control.Monad.State.Lazy


type Cmd  = (Event, Table)

appCmd :: [Event] -> Table -> Table 
appCmd [] t  = t
appCmd es t = snd $ execState (sequence $ map execEvent' es) (head es,t)


execEvent' :: Event -> State Cmd ExpToken
execEvent' e = do {(event, table) <- get; put (e,table) ; changeEQ' changePos' "show"} 


changeEQ' :: (ExpToken ->  State Cmd ExpToken) -> String  ->  State Cmd ExpToken
changeEQ'  f i = 	do
							(e,t) <- get
							let (r,p) = fromJust $ M.lookup i t
							exp <- f r
							modify (updateTable p exp i)
							return exp

updateTable :: Pos -> ExpToken -> String -> Cmd -> Cmd
updateTable p exp s (e,t)  = (e,  M.update (\_->Just (exp,p)) s t)



							

changePos' :: ExpToken ->  State Cmd ExpToken
changePos' (FuncT w  i [(ArrT  ap aw es)]) 
							= do
								(event, table) <- get
								let (a,(e:b)) = splitAt (_pos event) es
								newE <- changeExp'  e
							  	return (FuncT w i [(ArrT ap aw  (a ++ [newE] ++ b))])
							  	
							  		

changeExp' :: ExpToken -> State Cmd ExpToken
changeExp'  exp@(VarT  (IdT   p  w i )) = changeEQ' changeExp' i >> return exp
changeExp'  exp = getTableData' exp

getTableData' :: ExpToken -> State Cmd ExpToken
getTableData' t = changeFunctionArg' 0 appEvent' t 

changeFunctionArg' :: Int -> ( ExpToken -> State Cmd ExpToken) ->  ExpToken -> State Cmd ExpToken
changeFunctionArg' i f exp@(FuncT ap w es) = 
											do
												new <- (f (zs!! i))	
												return $ FuncT ap w ([] ++ [new] ++ (tail zs)) 
													where
														(ys,zs) = splitAt i es 
appEvent' :: ExpToken -> State Cmd ExpToken
appEvent' exp@(VarT  (IdT   vp  w i ))  =  changeEQ' (appEventType') i >> return exp
appEvent' exp = appEventType' exp

changeData' :: ExpToken -> State Cmd ExpToken
changeData'  e = do
				(e,t) <- get 
				return $ evalData $ now' e 


appEventType' :: ExpToken -> State Cmd ExpToken
appEventType'  exp = do
						(e,t) <- get 
						case (_type e) of
							CreateRow -> createRow'  exp
							RemoveRow -> removeRow'  exp
							Change -> changeCol'  exp

changeCol' :: ExpToken -> State Cmd ExpToken
changeCol' exp@(ArrT ap w es) = 	do
									(event,_) <- get
									let (a,(e:b)) = splitAt (number' event) es
									newE <- changeRow' e
  									return ((ArrT ap w (a ++ [newE] ++ b)))
							  		

							  											
changeRow' :: ExpToken -> State Cmd ExpToken
changeRow'  e@(ArrT ap w es) =  do {(event , table) <- get ;
					     			if ((index' event) >= (length es)) then addCell'  e else changeCell'  e}
changeRow'  exp@(VarT  (IdT   vp  w i ))  = changeEQ' (changeRow') i >> return exp
 

createRow' ::  ExpToken -> State Cmd ExpToken
createRow'  e@(ArrT ap w es) = do 
									(event, table) <- get
									return $ ArrT ap w (fmap (addEmptyCell $ index' event) es)
createRow'  e =error $ show e

removeRow' :: ExpToken -> State Cmd ExpToken
removeRow'   e@(ArrT ap w es) = do 
									(event, table) <- get
									return $ ArrT ap w (fmap (removeCell $ index' event) es)
removeRow'   e =error $ show e



changeCell' :: ExpToken -> State Cmd ExpToken
changeCell'  e@(ArrT ap w es) =do
								(event, table) <- get
								let (a,(e:b)) = splitAt (index' event) es
								newE <- changeData' e
								return ((ArrT ap w  (a ++ [newE] ++ b)))
														  		


addCell' ::  ExpToken -> State Cmd ExpToken
addCell' e@(ArrT ap w es) =do
							(event, table) <- get
							return ((ArrT ap w  (es ++ [evalData $ now' event])))
														  		








evalData :: String -> ExpToken
evalData s = either (\x -> StrT (0,0) ("","") "") (id) (evalParse atomicT s)  




removeCell :: Int -> ExpToken -> ExpToken
removeCell  p e@(ArrT ap w es) = ArrT ap w (ys ++ (tail zs))
								 where (ys,zs) = splitAt p es  

addEmptyCell :: Int -> ExpToken -> ExpToken
addEmptyCell  p e@(ArrT ap w es) =  let (ys,zs) = splitAt p es   in ArrT ap w  (ys ++ [createString] ++ zs)



createString :: ExpToken
createString = StrT  (0,0) ("","")  "" 




getArray :: ExpToken -> [ExpToken]
getArray a@(ArrT  p w es) = es

getFunction :: ExpToken -> [ExpToken]
getFunction f@(FuncT  p w es) = es

getVarName :: ExpToken -> String
getVarName (VarT  (IdT   p  w i )) = i




