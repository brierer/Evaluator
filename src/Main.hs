{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Main where

import Eval.Parser
import Eval.MultiPass
import Eval.Marshall
import Eval.Engine
import Eval.AppCmd

import Data.EvalCmd
import Data.Eval
import Data.Map as M
import Data.ExpObj
import Data.ExpToken
import Data.Maybe
import Data.EvalError
import Data.EvalResult

import Data.List
import Data.Aeson
import Data.Time.Clock
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy 

import Network.AMQP
import Database.Redis as Redis

main = do
    --connRedis <- connect defaultConnectInfo {connectHost = "pub-redis-14381.us-east-1-3.1.ec2.garantiadata.com", connectPort = PortNumber 14381 , connectAuth = Just $ "0UbTImi5I9qQ9ebQ"}--"70.83.162.27"
    connRedis <- connect defaultConnectInfo
    connRedis <- connect defaultConnectInfo  
    consumer $  saveMessage connRedis


consumer :: (String -> Channel -> IO () ) ->  IO ()
consumer f = do
            conn <- openConnection "localhost" "/" "guest" "guest"
           --conn <- openConnection "107.170.167.54" "/" "guest" "guest"
           --conn <- openConnection "tiger.cloudamqp.com" "vidvjemc" "vidvjemc" "27f27zSadNC1KCEfEJoSrsSDP80Vbtrn"
           -- conn <- openConnection' "lean-fiver-20.bigwig.lshift.net" 11022 "vndrShegf7N4" "5mPGLSH5" "-JSed3pUDdfCEUR9i-Bz1dXwZTtb7iGA"
            chan <- openChannel conn        
            declareQueue chan newQueue {queueName = "queue"}
            consumeMsgs chan "queue" NoAck (\(m,e) -> f (BL.unpack $ msgBody m) chan) 
            getLine -- wait for keypress
            closeConnection conn
            putStrLn "connection closed"        
        



saveMessage :: Redis.Connection -> String -> Channel -> IO ()
saveMessage con msg c=  do
                        putTime   
                        putStrLn $ show msg     
                        let cmd = (eitherDecode $ BL.pack msg) :: (Either String EvalCmd)
                        let key' = either id _key  cmd 
                        let eq' = either id _eq  cmd
                        let event' = either (show.id) (show._event) cmd
                        putStrLn $ show $ either (B.pack) evaluate cmd
                        runRedis con $ do
                                            set (B.pack key') (either (B.pack) evaluate cmd)
                                          --world <- get (B.pack key)
                                              --liftIO $ print (world) 
                        return ()
                        putTime        


putTime :: IO ()
putTime =  do
            time <- getCurrentTime                
            putStrLn (show $ utctDayTime $  time)


evaluate  :: EvalCmd -> B.ByteString
evaluate s =  B.concat . BL.toChunks $ encode $ pprint s table $ resolveShow $ table
              where table = createTable (_eq s) (_event s) 

createTable  :: String -> [Event] -> Either EvalError (Eval Table)
createTable  s e =  (\x -> appCmd e <$> x) <$> (initValidate $ getAllFunctionsNames) <$> (evalParse progT s)


resolveShow a = (\x ->  ((lookupAndMarshall "show") <$> x)) <$> a

lookupAndMarshall :: String ->  Table -> Maybe (Eval ExpObj)
lookupAndMarshall s t =  applyMarshall  t $ (fst <$> M.lookup s t) 

applyMarshall :: Table -> Maybe ExpToken -> Maybe (Eval ExpObj)
applyMarshall t m = fmap (\x -> (marshallWith t x funcs))  m

pprint  :: EvalCmd -> (Either EvalError (Eval Table)) -> Either EvalError (Either EvalError (Maybe (Eval ExpObj))) -> EvalResult
pprint s	t (Right x) = case x of
		 			    (Right y) -> either (KO (pprintTable t)) (OK (pprintTable t)) (fromJust y) 
					    (Left y)  -> KO  (OK_Table$ _eq s)  y 
pprint s  t (Left  x) = KO (OK_Table $ _eq s) x 


pprintTable :: Either EvalError (Eval Table) -> TableSTR
pprintTable (Right t) = case t of 
                            (Right x) -> OK_Table $ (sortTable x)
                            (Left x)  -> KO_Table $ x
pprintTable (Left t) =  KO_Table $ t

sortTable :: Table -> String
sortTable t = init $ Data.List.foldr (\x y -> x ++ ['\n'] ++ y) "" $ Data.List.map stringify  $ ( Data.List.sortBy  (sortPos) (toList  t))

getPos :: (String, (ExpToken, Pos)) -> Pos
getPos (s,(t,p)) = p

sortPos :: (String, (ExpToken, Pos)) -> (String, (ExpToken, Pos)) -> Ordering 
sortPos x y = (Main.getPos $ x) `compare` (Main.getPos $ y)

stringify :: (String, (ExpToken, Pos)) -> String
stringify (s, (t, p)) = s ++ " = " ++ (stringifyToken t)




