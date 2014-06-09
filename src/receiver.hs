{-# OPTIONS -XOverloadedStrings #-}
import Network.AMQP
import Evaluator.EvalParse
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock
import Database.Redis as Redis
import Control.Applicative
import Data.Char
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans (liftIO)
import Control.Concurrent 

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
                        let key = fst $ break (==';')  msg 
                        let eq  = tail $ snd $ break (==';') msg
                        putStrLn eq
                        putStrLn $ show $ map ord eq
                        result <- runParse $ eq                
                        putStrLn result                        
                        runRedis con $ do
                                              set (B.pack key) (B.pack $ result)
                                          --world <- get (B.pack key)
                                              --liftIO $ print (world) 
                        return ()
                        putTime        


putTime :: IO ()
putTime =  do
            time <- getCurrentTime                
            putStrLn (show $ utctDayTime $  time)
