module Server
    ( mainServer
    ) where


import Types


import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import Network.Socket (Socket, SockAddr,
                       Family(AF_INET), SocketType(Stream), SocketOption(ReuseAddr),
                       sOMAXCONN, socket, bind, listen, accept,
                       connect, close, setSocketOption)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BS

import Control.Exception (handle, SomeException(..))

import Control.Applicative ((<*), (*>))
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.State.Lazy (StateT, lift, runStateT, get, put, modify)

import Control.Concurrent (Chan, newChan, readChan, writeChan, forkIO)

import qualified Data.Map.Strict as Map


configFile :: String
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

                _ <- forkIO $ pure () <* runStateT (stateLoop eventChan sendChan) 0
                _ <- forkIO $ recvLoop addr eventChan
                _ <- forkIO $ pure () <* runStateT (sendLoop n otherCfg sendChan) Map.empty
                cmdLoop eventChan

stateLoop :: Chan Event -> Chan SendMsg -> LamportClock IO ()
stateLoop eventChan sendChan = do
    --lift $ putStrLn "stateLoop"
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

recvLoop :: SockAddr -> Chan Event -> IO ()
recvLoop addr eventChan = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock addr
    listen sock sOMAXCONN

    handle (\(SomeException _) -> pure ()) $ fix $ \loop -> do
        (client, _) <- accept sock
        _ <- forkIO $ processRecvLoop client
        loop

    close sock
  where
    processRecvLoop :: Socket -> IO ()
    processRecvLoop sock = fix $ \loop -> do
        --putStrLn "recvLoop"
        -- TODO write something sane instead
        byteString <- recvLen sock 24
        when (BS.length byteString /= 0) $ do
            let msg = decode byteString
            writeChan eventChan $ EventRecv msg
            loop
        close sock

    recvLen :: Socket -> Int -> IO BS.ByteString
    recvLen sock len = do
        byteString <- recv sock $ fromIntegral len
        let receivedLength = fromIntegral $ BS.length byteString
        if receivedLength == 0
            then pure BS.empty
            else if receivedLength < len
            then do
                rest <- recvLen sock $ len - receivedLength
                pure $ BS.concat [byteString, rest]
            else pure byteString

sendLoop :: Int -> Config -> Chan SendMsg -> StateT (Map.Map Int Socket) IO ()
sendLoop n cfg sendChan = do
    --lift $ putStrLn "sendLoop"
    msg@(SendMsg _ to) <- lift $ readChan sendChan
    intToSock <- get

    case Map.lookup to intToSock of
        Nothing -> case Map.lookup to intToAddr of
            Nothing -> lift $ putStrLn $ "No process " ++ show to
            Just addr -> do
                sock <- lift $ socket AF_INET Stream 0
                success <- lift $ checkedIO $ connect sock addr
                if success
                    then do
                        put $ Map.insert to sock intToSock
                        checkedSend sock msg
                    else lift (putStrLn "Can't connect" *> close sock)
        Just sock -> checkedSend sock msg
    sendLoop n cfg sendChan

    -- TODO Quit condition? Close sockets?
    pure ()
  where
    intToAddr = Map.fromList cfg

    checkedIO :: IO () -> IO Bool
    checkedIO m = handle (\(SomeException _) -> pure False) (m *> pure True)

    checkedSend :: Socket -> SendMsg -> StateT (Map.Map Int Socket) IO ()
    checkedSend sock msg@(SendMsg _ to) = do
        success <- lift $ checkedIO $ sendAll sock $ encode msg
        when (not success) (modify $ Map.delete to)

cmdLoop :: Chan Event -> IO ()
cmdLoop chan = do
    cmdStr <- getLine
    case parseCmd cmdStr of
        Left  _   -> putStrLn "Incorrect command"
        Right cmd -> writeChan chan (EventCmd cmd)
    cmdLoop chan
