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


main :: IO ()
main = do
  cmdArgs <- execParser opts
  _ <- case interfaceFile cmdArgs of
         Just path -> rawSystem "ghc" $ ["--show-iface "] ++ [show path]
         _         -> return ExitSuccess

  reverseProxy' <- return $ reverseProxy cmdArgs

  buildFile' <- return $ case buildFile cmdArgs of
                              Just file -> file
                              _ -> "Application.hs"

  runFunction' <- return $ case runFunction cmdArgs of
                               Just module' -> module'
                               _ -> "develMain"

  buildAndRun buildFile' runFunction' reverseProxy'

  where opts :: ParserInfo CmdArgs
        opts = info (helper <*> cmdArgs)
                (fullDesc
                 <> progDesc "For WAI complaint haskell web applications"
                 <> header "wai-devel: development server for haskell web applications." )
