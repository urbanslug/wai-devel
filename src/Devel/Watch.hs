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
                        (Added    _ _) -> return ()
                        (Modified _ _) -> (killThread threadId) >> notifyOnChange
                        (Removed  _ _) -> return ()
  mgr <- startManager
  watchTree mgr "." (const True) act
  forever $ threadDelay maxBound
