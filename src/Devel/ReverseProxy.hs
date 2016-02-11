{-|
Module      : Devel.ReverseProxy
Description : Reverse proxies and starts warp on localhost:\<PORT\>.
Copyright   : (c) 2015 Njagi Mwaniki
License     : MIT
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Reverse proxying and other network realated activities.
-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Devel.ReverseProxy 
( startReverseProxy
, createSocket
, createSocketSafe
, checkPort
, cyclePorts
) where

import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest), ProxyDest(ProxyDest), waiProxyTo)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (status200)


import Network.Wai.Handler.Warp (run)
import Control.Exception

import Network.Socket
import Data.Streaming.Network (bindPortTCP)

import Network.Wai (Application, responseLBS)

import Data.IORef
import Text.Hamlet
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)


startReverseProxy :: (Int, Int) -> IORef [String] -> IO ()
startReverseProxy  (fromProxyPort, toProxyPort) iStrLst = do
  mgr <- newManager defaultManagerSettings

  let onException' :: SomeException -> Application
      onException' _ _ respond =do
        lstt <- readIORef iStrLst
        respond $ responseLBS status200
                              [ ("content-type", "text/html")
                              , ("Refresh", "1")
                              ]
                              (renderHtml $(shamletFile "build.hamlet"))


      proxyApp = waiProxyTo
                      (\_ -> return $ WPRProxyDest $ ProxyDest "0.0.0.0" toProxyPort)
                      onException'
                      mgr

  -- runSettingsSocket defaultSettings sock proxyApp
  run fromProxyPort proxyApp


createSocketSafe :: Int -> IO (Socket, Int)
createSocketSafe port' = do
  port <- cyclePorts port'
  sock <- createSocket port
  return (sock, port)

createSocket :: Int -> IO Socket
createSocket port = do
  sock <- bindPortTCP port "*4"

  -- Tell the OS *not* to reserve the socket after your program exits.
  setSocketOption sock ReuseAddr 1

  return sock


-- Check whether a port is available to bind to.
checkPort :: Int -> IO Bool
checkPort port = do
    es <- try $ bindPortTCP port "*4"
    case es of
        Left (_ :: IOException) -> return False
        Right s -> do
            sClose s
            return True

cyclePorts :: Int -> IO Int
cyclePorts p = do
  let port = p + 1
  portAvailable <- checkPort port
  case portAvailable of
    True -> return port
    _ -> cyclePorts port
