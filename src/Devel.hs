module Main where

import Devel.Watch (compile)
import Devel.ReverseProxy (runServer)
import Control.Concurrent (forkIO)

main :: IO ()
main = do 
  _ <- forkIO compile
  runServer
