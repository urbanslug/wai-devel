{-# LANGUAGE OverloadedStrings #-}
module Devel.ReverseProxy (runServer) where

import Network.Wai (Application)
import Network.HTTP.ReverseProxy (WaiProxyResponse(WPRProxyDest), ProxyDest(ProxyDest), waiProxyTo, defaultOnExc)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)

reverseProxy :: IO Application
reverseProxy = do
  mgr <- newManager defaultManagerSettings
  return $ waiProxyTo
         (const $ return $ WPRProxyDest $ ProxyDest "localhost" 3001)
         defaultOnExc
         mgr
                  
runServer :: IO ()
runServer = do
  appl <- reverseProxy
  run 3000 appl
