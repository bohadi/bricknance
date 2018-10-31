{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
#-}

module Backend (
  runStream
) where

import Network.Socket (withSocketsDo)
import Network.WebSockets (ClientApp, receiveData, sendTextData, Connection)
import Wuss (runSecureClient)

import Data.Aeson (eitherDecode)

import Control.Monad (forever, void, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (ThreadId, forkIO)

import Data.Char (toLower)
import qualified Data.Text    as T (Text, pack, unpack, null)
import qualified Data.Text.IO as T (putStrLn, getLine)
import qualified Data.ByteString.Lazy.Char8 as BC (putStrLn)

import Brick.BChan (BChan, writeBChan)

import Util

-- TODO forkIO loop this is the wrong pattern
--ferryOverStream :: BChan StreamEvent -> Connection -> ClientApp ()
ferryOverStream bfc conn = do
  threadid <- forkIO $ forever $ do
    msg <- receiveData conn
    case (eitherDecode msg :: Either String StreamEvent) of
      --Right payload -> liftIO $ putStrLn $ show payload
      Right payload -> writeBChan bfc payload
      Left errMsg -> liftIO $ putStrLn $ "errMsg: " <> errMsg
  loop
  return (threadid, conn)
  where
    loop =
      T.getLine >>= \line ->
        unless (T.null line) $
          sendTextData conn line >> loop

runStream :: BChan StreamEvent -> BChan CmdEvent -> IO (ThreadId, Connection)
runStream bfc _ = runSecureClient "stream.binance.com" 9443 path app
  where path = "/stream?streams=" <> (concat $ show <$> defaultEndpoints)
        app  = ferryOverStream bfc
