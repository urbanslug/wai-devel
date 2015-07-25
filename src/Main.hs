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

 buildAndRun 

 where opts :: ParserInfo CmdArgs
       opts = info (helper <*> cmdArgs)
               (fullDesc
                <> progDesc "A development server for haskell web applications."
                <> header "Yesod-devel." )
