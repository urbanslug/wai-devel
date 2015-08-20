{-|
Module      : Devel
Description : An entry point for GHC to compile yesod-devel.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Devel where

import Devel.Build (build)
import Devel.ReverseProxy
import IdeSession (sessionConfigFromEnv)
import System.Environment (lookupEnv, setEnv)

buildAndRun :: FilePath -> String ->  Bool -> IO ()
buildAndRun buildFile runFunction reverseProxy = do
  config <- sessionConfigFromEnv
  
  -- If port isn't set we assume port 3000
  mPort <- lookupEnv "PORT"
  let srcPort = case mPort of
                  Just p -> read p :: Int
                  _ -> 3000

  -- Gives us a port to reverse proxy to.
  destPort <- cyclePorts srcPort

  case reverseProxy of
    True -> setEnv "PORT" (show destPort)
    False -> setEnv "PORT" (show srcPort)

  case reverseProxy of
    True -> build buildFile runFunction reverseProxy config (srcPort, destPort)
    False -> build buildFile runFunction reverseProxy config (destPort, srcPort)

cyclePorts :: Int -> IO Int
cyclePorts p = do
  let port = p + 1
  portAvailable <- checkPort port
  case portAvailable of
    True -> return port
    _ -> cyclePorts port
