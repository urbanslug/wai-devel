{-# LANGUAGE OverloadedStrings #-}
module Devel.WebSockets 
( -- runWsServer
handleConnection
) where

import qualified Network.WebSockets as WS
import qualified Data.Text as T


-- What if this was in compile before we run the updateSession?
runWsServer :: T.Text -> IO ()
runWsServer msg = WS.runServer "127.0.0.1" 5002 (handleConnection msg)


handleConnection :: T.Text -> WS.PendingConnection -> IO ()
handleConnection msg pending = do
  conn <- WS.acceptRequest pending
  WS.sendTextData conn (msg :: T.Text)
  WS.sendClose conn ("Closing" :: T.Text)
