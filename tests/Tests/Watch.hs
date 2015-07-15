module Tests.Watch where

import Test.HUnit

import Devel.Watch


import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Control.Concurrent (forkIO, threadDelay, killThread)
import System.Directory (removeFile)

-- Why is this failing?
testWatch :: Test
testWatch = TestCase $ do
  isDirty <- newTVarIO False
  writeFile "modify.txt" "New file."
  threadId <- forkIO $ watch isDirty
  appendFile "modify.txt" "\n\nFile Modified."
  isDirty' <- atomically $ readTVar isDirty
  _ <- killThread threadId
  removeFile "modify.txt"
  assertEqual
    "Watches for file modifications in cwd"
    True
    isDirty'

testCheckForChange :: Test
testCheckForChange = TestCase $ do
  isDirty <- newTVarIO True
  _ <- checkForChange isDirty
  isDirty' <- atomically $ readTVar isDirty
  assertEqual
    "Modifies TVar isDirty."
    False
    isDirty'
