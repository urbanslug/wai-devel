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
module Devel.ReverseProxy (runServer, createSocket) where

import Network.Wai (Application, responseBuilder)
import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest), ProxyDest(ProxyDest), waiProxyTo)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Control.Exception

import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import IdeSession (SourceError, errorMsg)
import Data.Text (unpack)
import Network.Socket

import Devel.Types

import System.Environment (getEnv)
import Data.ByteString.Char8 (pack)

-- | run the warp server
runServer :: [SourceError'] -> Socket -> IO ()
runServer errorList sock =
  reverseProxy errorList >>= runSettingsSocket defaultSettings sock

-- | Does reverse proxying to localhost:3001
reverseProxy :: [SourceError'] -> IO Application
reverseProxy errorList = do

  port <- getEnv "wai_port"
  address <- getEnv "wai_address"

  mgr <- newManager defaultManagerSettings
  errorList' <- return errorList

  let error500 :: SomeException -> Application
      error500 _ _ respond = respond $
        responseBuilder
        status502
        [("content-type", "text/html; charset=utf-8")]
        (renderHtmlBuilder $(shamletFile "error.hamlet"))

  return $ waiProxyTo
         (const $ return $ WPRProxyDest $ ProxyDest (pack address) (read port))
         error500
         mgr

-- | Create the socket that we will use to communicate with
-- localhost:3000 here.
createSocket :: IO Socket
createSocket = do

  sock <- socket AF_INET Stream defaultProtocol

  -- Tell the OS *not* to reserve the socket after your program exits.
  setSocketOption sock ReuseAddr 1

  -- Bind the socket to localhost:3000 and listen.
  -- I wonder why I can't specify localhost instead of iNADDR_ANY
  bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
  listen sock 2
  return sock
