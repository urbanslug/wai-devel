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
  _ <- case interfaceFile cmdArgs of
         Just path -> rawSystem "ghc" $ ["--show-iface "] ++ [show path]
         _         -> return ExitSuccess

  -- Default reverse proxying is True
  isReverseProxy' <- return $ isReverseProxy cmdArgs

  buildFile' <- return $ case buildFile cmdArgs of
                              Just path' -> path'
                              _ -> "Application.hs"

  runFunction' <- return $ case runFunction cmdArgs of
                               Just function' -> function'
                               _ -> "develMain"

  buildAndRun buildFile' runFunction' isReverseProxy'
  where opts :: ParserInfo CmdArgs
        opts = info (helper <*> cmdArgs)
                (fullDesc
                 <> progDesc "For WAI complaint haskell web applications"
                 <> header "wai-devel: development server for haskell web applications." )
