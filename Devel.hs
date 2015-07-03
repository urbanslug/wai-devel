{-|
Module      : Main
Description : An entry point for GHC to compile yesod-devel.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Main where

import Devel.Build (build)
import Control.Concurrent

main :: IO ()
main = build
