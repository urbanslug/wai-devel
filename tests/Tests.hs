module Main where

import Test.Hspec

main :: IO Counts
main = runTestTT $ TestList []
