module Main where

import Devel.Watch
import System.FSNotify (StopListening)


main :: IO ()
main = notifyOnChange
