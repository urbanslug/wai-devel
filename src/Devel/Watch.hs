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
{-# LANGUAGE OverloadedStrings, CPP #-}
module Devel.Watch where

import IdeSession

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.FSNotify
import System.FSNotify.Devel
import Control.Monad      (forever)
import Control.Concurrent (threadDelay)

import Devel.Types
import Devel.Paths (getFilesToWatch)

# if __GLASGOW_HASKELL__ < 710
import Data.Text (unpack)
import Filesystem.Path.CurrentOS (toText)
import qualified Filesystem.Path as FSP
#endif

import System.Directory (getCurrentDirectory)
import System.FilePath (pathSeparator)


-- "Smart" file watching.
watch :: TVar Bool -> [FilePath] -> IO ()
watch isDirty includeTargets = do
  dir <- getCurrentDirectory
  let pathsToWatch = map (\fp -> dir ++ (pathSeparator: fp)) includeTargets
  manager <- startManagerConf defaultConfig
  _ <- watchTree manager "." (const True)
         -- Last argument to watchTree.
         (\event -> do
            let getPath :: Event -> FilePath
                getPath (Added fp _)    = fp
                getPath (Modified fp _) = fp
                getPath (Removed fp _)  = fp

                isModified = getPath event `elem` pathsToWatch
            atomically $ writeTVar isDirty isModified)

  _ <- forever $ threadDelay maxBound
  stopManager manager

-- | Runs in the current working directory. Watches when there's an error.
-- Watches for file changes for the specified file extenstions
-- When a change is found. It modifies isDirty to True.
watchErrored :: TVar Bool -> IO ()
watchErrored isDirty = do
  manager <- startManagerConf 
                  defaultConfig {confUsePolling= True}

  _ <- treeExtAny manager "." "hamlet"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "shamlet" (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "lucius"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "julius"  (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "hs"      (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "lhs"      (\_ -> atomically $ writeTVar isDirty True)
  _ <- treeExtAny manager "." "yaml"    (\_ -> atomically $ writeTVar isDirty True)

  _ <- forever $ threadDelay maxBound
  stopManager manager


checkForChange :: TVar Bool -> IO ()
checkForChange isDirty =
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False
 {-do
  atomically $ readTVar isDirty >>= \(isDirty', _) -> check isDirty'
  (_, fileChange) <- readTVarIO isDirty
  print fileChange
  atomically $ writeTVar isDirty (False, NoChange)
  return fileChange-}

checkForChangeErrored :: TVar Bool -> IO ()
checkForChangeErrored isDirty =
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False
