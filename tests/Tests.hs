module Main where

import Test.HUnit.Text
import Tests.Watch
import Test.HUnit.Base

main :: IO Counts
main = runTestTT $ TestList []
