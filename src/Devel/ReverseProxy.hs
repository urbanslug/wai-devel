{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Devel.ReverseProxy (runServer) where

import Network.Wai (Application, responseBuilder)
import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest), ProxyDest(ProxyDest), waiProxyTo)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Control.Exception

import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Blaze.Html (Html)

import Control.Concurrent
import Control.Monad (liftM)

reverseProxy :: [String] -> IO Application
reverseProxy errorList = do
  mgr <- newManager defaultManagerSettings
  errorList' <- return errorList
  let error500 :: SomeException -> Application
      error500 _ _ respond = respond $
        responseBuilder
        status502
        [("content-type", "text/html; charset=utf-8")]
        (renderHtmlBuilder $(shamletFile "error.hamlet"))
  return $ waiProxyTo
         (const $ return $ WPRProxyDest $ ProxyDest "localhost" 3001)
         error500
         mgr

runServer :: MVar Bool -> [String] -> IO ()
runServer bul' eL = do
  bul <- takeMVar bul'
  case bul of
    False -> reverseProxy eL >>= run 3000
    _ -> return ()
