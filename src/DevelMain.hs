{-|
Module      : Main
Description : The main module of the executable.
Copyright   : (c) 2015 Njagi Mwaniki
License     : MIT
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module DevelMain where

import Devel
import Devel.CmdArgs
import Options.Applicative

import Paths_wai_devel (version)
import Data.Version (showVersion)

develMain :: IO ()
develMain = do
  cmdArgs' <- execParser opts

  let isReverseProxy' = isReverseProxy cmdArgs'
      buildFile' = buildFile cmdArgs'
      watchDirectories' = filter (/= "") $ watchDirectories cmdArgs'
      runFunction' = runFunction cmdArgs'
      isPrintVersion = versionNumber cmdArgs'

  if isPrintVersion
    then putStrLn $ "wai-devel version: " ++ showVersion version -- print version number and exit.
    else buildAndRun buildFile' runFunction' watchDirectories' isReverseProxy'

  where opts :: ParserInfo CmdArgs
        opts = info (helper <*> cmdArgs)
                (fullDesc
                 <> progDesc "For WAI complaint haskell web applications"
                 <> header "wai-devel: development server for haskell web applications." )
