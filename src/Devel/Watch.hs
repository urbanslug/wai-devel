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
                        (Added    _path _) -> (killThread threadId) >> runBackend
                        (Modified _path _) -> (killThread threadId) >> runBackend
                        (Removed  _path _) -> (killThread threadId) >> runBackend
  mgr <- startManager
  watchDir mgr "." (const True) act
  forever $ threadDelay maxBound
