{-|
Module      : Devel.Watch
Description : Watch for changes in the current working direcory.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Actually checks only for modified files.
Added or removed files don't trigger new builds.
-}
{-# LANGUAGE OverloadedStrings #-}
module Devel.Watch where

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.FSNotify.Devel
import System.FSNotify
import Control.Monad      (forever)

import Control.Concurrent (threadDelay)

{-
-- | Watches for file modification in the current working directory.
--   When a change is found. It modifies isDirty to True.
watch :: TVar Bool -> IO ()
watch isDirty = do
  _ <- withManagerConf (defaultConfig {confDebounce=(Debounce 1000), confPollInterval=10000000}) watch'
  return ()
  where watch' :: WatchManager -> IO StopListening
        --  Watch for file changes in the current working directory.
        
        watch' mgr = do _ <- watchTree mgr "." (const True) detectChange -- (\_ -> atomically $ writeTVar isDirty True) -- detectChange
                        forever $ threadDelay maxBound
        -- type Action = Event -> IO ()
        detectChange :: Event -> IO ()
        detectChange event = 
          case event of
            (Added    _ _)  -> return ()
            (Modified _ _) -> atomically $ writeTVar isDirty True
            (Removed  _ _) -> return ()
-}

-- | Runs in the current working directory 
-- Watches for file changes for the specified file extenstions
-- When a change is found. It modifies isDirty to True.
watch :: TVar Bool -> IO ()
watch isDirty = do
  manager <- startManagerConf defaultConfig

  treeExtExists manager "." "hamlet"  (\_ -> atomically $ writeTVar isDirty True)
  treeExtExists manager "." "shamlet" (\_ -> atomically $ writeTVar isDirty True)
  treeExtExists manager "." "lucius"  (\_ -> atomically $ writeTVar isDirty True)
  treeExtExists manager "." "hs"      (\_ -> atomically $ writeTVar isDirty True)
  treeExtExists manager "." "yaml"    (\_ -> atomically $ writeTVar isDirty True)
  
  forever $ threadDelay maxBound
  stopManager manager

checkForChange :: TVar Bool -> IO ()
checkForChange isDirty = do
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False
