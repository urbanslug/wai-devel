{-|
Module      : Devel
Description : An entry point for GHC to compile yesod-devel.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Devel where

import Devel.Build (build)
import IdeSession (sessionConfigFromEnv)

buildAndRun :: FilePath -> String ->  Bool -> IO ()
buildAndRun buildFile runFunction reverseProxy =
  sessionConfigFromEnv >>= \config -> build buildFile runFunction reverseProxy config
