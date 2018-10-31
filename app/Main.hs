{-# LANGUAGE
    ScopedTypeVariables
  , OverloadedStrings
#-}

module Main where

import Frontend
import Backend
import Util

import Data.Text (Text)
import Network.WebSockets (sendClose)
import Brick.BChan (BChan, newBChan)
import Control.Monad (void, (>>))
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  (bfc :: BChan StreamEvent) <- newBChan 10
  (fbc :: BChan CmdEvent)    <- newBChan  5
  (theadId, conn) <- runStream bfc fbc
  endS            <- runTui bfc fbc
  --killThread theadId
  -- TODO why does ghc give an error on quit: modifyFdOnce bad file descriptor
  sendClose conn ("Bye!" :: Text)
  threadDelay 3000000
  putStrLn "goodbye"
