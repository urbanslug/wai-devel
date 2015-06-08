module Main where

import Devel.Watch (notifyOnChange)
import Devel.ReverseProxy (runServer)
-- import System.FSNotify (StopListening)
-- import System.Process (runCommand)
import Control.Concurrent (forkIO)

main :: IO ()
main = do 
  _ <- forkIO notifyOnChange
  runServer
