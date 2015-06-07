{-# LANGUAGE OverloadedStrings #-}
module Devel.Watch where

import System.FSNotify
import Control.Monad      (forever)
import Devel.Run          (runBackend)
import Control.Concurrent (threadDelay, forkIO, killThread)

notifyOnChange :: IO ()
notifyOnChange = do
  threadId <- forkIO runBackend
  let act = \event -> case event of
                        (Added    _path _) -> (killThread threadId) >> runBackend >> putStrLn ("Reloading, new file.")
                        (Modified _path _) -> runBackend >> putStrLn ("Reloading, modification.")
                        (Removed  _path _) -> runBackend >> putStrLn ("Removed.") 
  mgr <- startManager
  watchDir mgr "." (const True) act
  forever $ threadDelay maxBound
