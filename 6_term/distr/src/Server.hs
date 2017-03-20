module Server where
    ( mainServer
    ) where


import Types


import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy (send, recv, sendAll)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BS

import Control.Exception (throw, catch, handle, SomeException(..))

import Control.Applicative ((<*))
import Control.Monad (liftM, when, zipWithM)
import Control.Monad.Fix (fix)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class

import Control.Concurrent
import Control.Concurrent.Chan

import qualified Data.Map.Strict as Map
import Data.Tuple (swap)

import Text.Megaparsec (parse)


configFile = "process.cfg"

mainServer :: IO ()
mainServer = do
    args <- getArgs
    when (null args) $ putStrLn "Process id not specified" >> exitFailure
    case args of
        []      -> putStrLn "Process id not specified" >> exitFailure
        str : _ -> case readMaybe str of
            Nothing -> putStrLn "Incorrect process id" >> exitFailure
            Just n  -> runProcess n

data Event
    = EventCmd Cmd
    | EventRecv ReceivedMsg

runProcess :: Int -> IO ()
runProcess n = do
    configLines <- readFile configFile
    case parseConfig configLines of
        Left  _   -> putStrLn ("Incorrect " ++ configFile) >> exitFailure
        Right cfg -> case lookup n cfg of
            Nothing   -> putStrLn ("No process " ++ show n ++ " in " ++ configFile) >> exitFailure
            Just addr -> do
                let otherCfg = filter ((/= n) . fst) cfg

                eventChan <- newChan
                sendChan <- newChan

                forkIO $ pure () <* runStateT (stateLoop eventChan sendChan) 0
                forkIO $ recvLoop addr otherCfg eventChan
                forkIO $ pure () <* runStateT (sendLoop n otherCfg sendChan) Map.empty
                cmdLoop eventChan

stateLoop :: Chan Event -> Chan SendMsg -> LamportClock IO ()
stateLoop eventChan sendChan = do
    lift $ putStrLn "stateLoop"
    event <- lift $ readChan eventChan
    case event of
        EventCmd (Cmd to int) -> do
            clock <- lamportSend
            let msg = SendMsg (Msg int clock) to
            lift $ writeChan sendChan msg
        EventRecv (ReceivedMsg (Msg int clock) from) -> do
            newClock <- lamportReceive clock
            lift $ putStrLn $ show $ ReceivedMsg (Msg int newClock) from
    stateLoop eventChan sendChan

recvLoop :: SockAddr -> Config -> Chan Event -> IO ()
recvLoop addr cfg eventChan = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock addr
    listen sock sOMAXCONN

    otherSocks <- mapM (const $ liftM fst $ accept sock) cfg
    mapM_ (forkIO . processRecvLoop) otherSocks

    close sock
  where
    processRecvLoop :: Socket -> IO ()
    processRecvLoop sock = do
        fix $ \loop -> do
            putStrLn "recvLoop"
            -- TODO write something sane instead
            byteString <- recv sock 24
            when (BS.length byteString == 0) $ error "closed"
            let (n, int, clock) = decode byteString
            writeChan eventChan $ EventRecv $ ReceivedMsg (Msg int clock) n
            loop
            -- TODO Quit condition? Close sockets?

    --recvLen :: Socket -> Int -> IO BS.ByteString
    --recvLen sock len = do
    --    byteString <- recv sock $ fromIntegral len
    --    let receivedLength = fromIntegral $ BS.length byteString
    --    if receivedLength < len
    --        then do
    --            rest <- recvLen sock $ len - receivedLength
    --            pure $ BS.concat [byteString, rest]
    --        else pure byteString

sendLoop :: Int -> Config -> Chan SendMsg -> StateT (Map.Map Int Socket) IO ()
sendLoop n cfg sendChan = do
    lift $ putStrLn "sendLoop"
    (SendMsg (Msg int clock) to) <- lift $ readChan sendChan
    intToSock <- get
    when (not $ Map.member to intToSock) $ do
        let maybeAddr = Map.lookup to intToAddr
        case maybeAddr of
            Nothing -> lift $ putStrLn $ "No process " ++ show to
            Just addr -> do
                sock <- lift $ socket AF_INET Stream 0
                -- TODO handle exception
                lift $ connect sock addr
                put $ Map.insert to sock intToSock
    intToSock <- get
    case Map.lookup to intToSock of
        Nothing -> lift $ putStrLn "No such process."
        Just sock -> do
            let byteString = encode (n, int, clock)
            lift $ sendAll sock byteString

    sendLoop n cfg sendChan

    -- TODO Quit condition? Close sockets?
    pure ()
  where
    intToAddr = Map.fromList cfg

cmdLoop :: Chan Event -> IO ()
cmdLoop chan = do
    cmd <- getLine
    case parse cmdParser "" cmd of
        Left  e   -> putStrLn ("Incorrect command: " ++ show e)
        Right cmd -> writeChan chan (EventCmd cmd)
    cmdLoop chan
