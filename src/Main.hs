{-|
Module      : Main
Description : So that it can work as an executable.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Main where

import Devel
import Devel.Args
import Options.Applicative
import System.Process (rawSystem)
import System.Exit (ExitCode(ExitSuccess))

import System.Environment (setEnv)

main :: IO ()
main = do
  cmdArgs <- execParser opts

  let isReverseProxy' = isReverseProxy cmdArgs
      buildFile' = buildFile cmdArgs
      runFunction' = runFunction cmdArgs

  buildAndRun buildFile' runFunction' isReverseProxy'
  where opts :: ParserInfo CmdArgs
        opts = info (helper <*> cmdArgs)
                (fullDesc
                 <> progDesc "For WAI complaint haskell web applications"
                 <> header "wai-devel: development server for haskell web applications." )
