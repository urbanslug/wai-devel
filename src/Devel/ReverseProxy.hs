{-# LANGUAGE OverloadedStrings #-}
module Devel.ReverseProxy (runServer) where

import Network.Wai
import Network.HTTP.ReverseProxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)

reverseProxy :: IO Application
reverseProxy = do
  mgr <- newManager defaultManagerSettings
  return $ waiProxyTo
         (const $ return $ WPRProxyDest $ ProxyDest "localhost" 4000)
         defaultOnExc
         mgr
         
runServer :: IO ()
runServer = do
  appl <- reverseProxy
  putStrLn "Starting devel server http://localhost:3000"
  run 3000 appl
