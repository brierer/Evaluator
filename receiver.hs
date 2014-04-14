{-# OPTIONS -XOverloadedStrings #-}
import Network.AMQP
import Evaluator.EvalParse
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock
import Database.Redis as Redis
import Control.Applicative
import qualified Data.ByteString.Char8 as B

main = do
    connRedis <- connect defaultConnectInfo 	  
    consumer $  saveMessage connRedis


consumer :: (String -> IO () ) ->  IO ()
consumer f = do
	    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
	    chan <- openChannel conn	
	    declareQueue chan newQueue {queueName = "queue"}
	    consumeMsgs chan "queue" NoAck (\(m,e) -> f $ BL.unpack $ msgBody m) 
	    getLine -- wait for keypress
   	    closeConnection conn
            putStrLn "connection closed"	
	



saveMessage :: Redis.Connection -> String -> IO ()
saveMessage con msg =  do
			putTime	
			let key = fst $ break (==';')  msg 
			let eq  = tail $ snd $ break (==';') msg
			putStrLn eq
			result <- runParse $ eq		
			putStrLn result			
			runRedis con $ do
      					set (B.pack key) (B.pack $ result) 
		                        return ()
			putTime	


putTime :: IO ()
putTime =  do
	    time <- getCurrentTime
	    putStrLn (show $ utctDayTime $  time)
