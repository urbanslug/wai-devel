module Main where

import Devel.Watch (doCompile)
import Control.Concurrent

main :: IO ()
main = do
  bul <- newMVar False
  doCompile bul
