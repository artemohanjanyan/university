{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Types
    ( ConfigLine
    , Config
    , parseConfig

    , LamportClock
    , lamportSend
    , lamportReceive

    , Cmd(..)
    , parseCmd
    , Msg(..)
    , SendMsg(..)
    , ReceivedMsg(..)
    ) where


import Network.Socket

import Data.Binary (Binary)
import GHC.Generics (Generic)

import Control.Monad.State.Lazy

import Control.Applicative ((*>))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Lexer (integer)


type ConfigLine = (Int, SockAddr)
type Config = [ConfigLine]

configLineParser :: Parser ConfigLine
configLineParser = do
    string "process."
    n <- integer <?> "process id"
    string "="
    let octet = integer <?> "octet"
    ipHead <- octet
    ipTail <- count 3 (string "." >> octet)
    string ":"
    port <- integer <?> "port"
    pure (fromIntegral n, SockAddrInet (fromIntegral port) (calcIp $ ipHead : ipTail))
  where
    calcIp ints =
        let [ip1, ip2, ip3, ip4] = map fromIntegral ints
        in tupleToHostAddress (ip1, ip2, ip3, ip4)

parseConfig :: String -> Either (ParseError Char Dec) Config
parseConfig str = mapM (parse configLineParser "config") (lines str)

type LamportClock = StateT Int

lamportSend :: Monad m => LamportClock m Int
lamportSend = do
    modify succ
    get

lamportReceive :: Monad m => Int -> LamportClock m Int
lamportReceive received = do
    n <- get
    let new = 1 + max n received
    put new
    pure new

data Cmd = Cmd
    { cmdTo :: Int
    , cmdInt :: Int
    }

cmdParser :: Parser Cmd
cmdParser = do
    string "send to:"
    to <- integer <?> "process id"
    string " msg:"
    sign <- (string "-" *> pure (0 -)) <|> pure id
    msg <- integer <?> "message"
    pure $ Cmd
        { cmdTo = (fromIntegral to)
        , cmdInt = (fromIntegral $ sign msg)
        }

parseCmd :: String -> Either (ParseError Char Dec) Cmd
parseCmd = parse cmdParser ""

data Msg = Msg
    { msgInt :: Int
    , msgClock :: Int
    } deriving (Generic)
instance Binary Msg

data SendMsg = SendMsg
    { sendMsg :: Msg
    , sendTo :: Int
    } deriving (Generic)
instance Binary SendMsg

data ReceivedMsg = ReceivedMsg
    { receivedMsg :: Msg
    , receivedFrom :: Int
    } deriving (Generic)
instance Binary ReceivedMsg

instance Show ReceivedMsg where
    show (ReceivedMsg (Msg msg clock) from) =
            "received" ++
            " from:" ++ show from ++
            " msg:" ++ show msg ++
            " time:" ++ show clock
