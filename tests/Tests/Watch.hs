module Tests.Watch where

import Test.HUnit

import Devel.Watch
import Devel.Types

-- import Control.Concurrent.STM.TVar
-- import Control.Monad.STM
import GHC.Conc

-- import Control.Concurrent (forkIO, threadDelay, killThread)
import System.Directory (removeFile)

-- Why is this failing?
testWatch :: Test
testWatch = TestCase $ do
  isDirty <- newTVarIO (False, NoChange)
  writeFile "modify.txt" "New file."
  threadId <- forkIO $ watch isDirty ["modify.txt"]
  appendFile "modify.txt" "\n\nFile Modified."
  isDirty' <- atomically $ readTVar isDirty
  _ <- killThread threadId
  removeFile "modify.txt"
  assertEqual
    "Watches for file modifications in the current working directory"
    (True, (Modification "modify.txt"))
    isDirty'

testCheckForChange :: Test
testCheckForChange = TestCase $ do
  isDirty <- newTVarIO (False, NoChange)
  _ <- checkForChange isDirty
  isDirty' <- atomically $ readTVar isDirty
  assertEqual
    "Modifies TVar isDirty."
    (False, NoChange)
    isDirty'
