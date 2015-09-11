{-# LANGUAGE OverloadedStrings #-}
module Devel.WebSockets
( printWithWebSocket
) where

import qualified Data.ByteString.Char8 as BS

import qualified Network.WebSockets as WS
import Network.Socket      (withSocketsDo, accept ) -- Socket
import Data.IORef
import IdeSession (UpdateStatus)
-- import GHC.Conc (forkIO)

printWithWebSocket :: IORef (Maybe WS.Connection) -> UpdateStatus -> IO ()
printWithWebSocket iMaybeConn status = do
  maybeConn <- readIORef iMaybeConn
  case maybeConn of
    Just conn -> WS.sendTextData conn $ BS.pack $ show status
    Nothing -> withSocketsDo $ do
      sock' <- WS.makeListenSocket "127.0.0.1" 5000
      (sock, _) <- accept sock'
      pending <- WS.makePendingConnection sock WS.defaultConnectionOptions
      conn <- WS.acceptRequest pending
      _ <- writeIORef iMaybeConn (Just conn)
      WS.sendTextData conn $  BS.pack $ show status
    {-
    Nothing -> do
      let serverApp :: WS.ServerApp
          serverApp pending = do
            conn <- WS.acceptRequest pending
            _ <- writeIORef iMaybeConn (Just conn)
            WS.sendTextData conn $  BS.pack $ show status
      _ <- forkIO $ WS.runServer "0.0.0.0" 5000 serverApp 
      return ()
    -}
    {-
    
   -}

{-
-- Client
-- Use forever to run client until connection is closed by the server
client :: WS.ClientApp ()
client conn = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  print msg

runC :: IO ()
runC = withSocketsDo $ WS.runClient "127.0.0.1" 5000 "/" client
-}
