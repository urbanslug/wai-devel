{-|
Module      : Devel.ReverseProxy
Description : Reverse proxyies and starts warp on localhost:3000.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Networking things are expected to be done here.
-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Devel.ReverseProxy (runServer, createSocket, checkPort) where

import Network.Wai (Application, responseBuilder)
import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest), ProxyDest(ProxyDest), waiProxyTo)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Control.Exception

import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import Network.Socket
import Data.Streaming.Network
import Devel.Types


-- | run the warp server
runServer :: [SourceError'] -> Socket -> Int -> IO ()
runServer errorList sock destPort = do
  app <- reverseProxy errorList destPort
  runSettingsSocket defaultSettings sock app


-- | Does reverse proxying to localhost given port
reverseProxy :: [SourceError'] -> Int -> IO Application
reverseProxy errorList destPort = do

  mgr <- newManager defaultManagerSettings
  errorList' <- return errorList

  let error500 :: SomeException -> Application
      error500 _ _ respond = respond $
        responseBuilder
        status502
        [("content-type", "text/html; charset=utf-8")]
        (renderHtmlBuilder $(shamletFile "error.hamlet"))

  return $ waiProxyTo
         (const $ return $ WPRProxyDest $ ProxyDest "127.0.0.1" destPort)
         error500
         mgr


-- | Create the socket that we will use to communicate with
-- localhost:3000 here.
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
