{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( mainServer
    ) where

import Network.Socket
import System.IO
import Control.Exception as E
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)

mainServer :: IO ()
mainServer = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 2539 iNADDR_ANY)
    listen sock sOMAXCONN

    chan <- newChan
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    putStrLn "Running..."
    mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO $ runConn conn chan msgNum
    mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, sockAddr) chan msgNum = do
    putStrLn $ case sockAddr of
        SockAddrInet port _ -> show port
        _                   -> show sock

    let broadcast msg = writeChan chan (msgNum, msg)

    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering

    hPutStrLn handle "Hi, what's your name?"
    name <- liftM init (hGetLine handle)
    broadcast $ "--> " ++ name ++ " entered chat."
    hPutStrLn handle $ "Welcome, " ++ name ++ "!"

    commLine <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn handle line
        loop

    E.handle (\(SomeException _) -> pure ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine handle)
        case line of
             "quit" -> hPutStrLn handle "Bye!"
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader
    broadcast $ "<-- " ++ name ++ " left."
    hClose handle
