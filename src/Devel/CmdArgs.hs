{-|
Module      : Devel.Args
Description : For handling command line arguments.
Copyright   : (c) 2015 Njagi Mwaniki
License     : MIT
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

We handle command line arguments for yesod devel here.
-}
module Devel.CmdArgs
(  cmdArgs
,  CmdArgs (..)
) where

import           Data.List.Split     (splitOn)
import           Options.Applicative


-- | All arguments are optional.
data CmdArgs = CmdArgs
  { buildFile        :: FilePath
  , runFunction      :: String
  , watchDirectories :: [String]
  , versionNumber    :: Bool -- Default: False
  , isReverseProxy   :: Bool -- Default: True
  } deriving (Show, Eq)

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
        <$>  strOption
             (long "path"
               <> short 'p'
               <> value "Application.hs"
               <> metavar "FILEPATH"
               <> help "The file with the function you want to run. Default is `Application.hs`.")
        <*>  strOption
               (long "function"
                 <> short 'f'
                 <> value "develMain"
                 <> metavar "FUNCTION"
                 <> help "The function you want run. Default is `develMain`.")
        <*>  (splitOn "," <$> strOption
                                (long "watch-directories"
                                  <> short 'w'
                                  <> value []
                                  <> metavar "DIRECTORY-LIST"
                                  <> help "A comma-separated list of directories which have files we want to watch for changes in"))
        <*> flag False True
              (long "version"
                <> short 'v'
                <> help "Print the version of wai-devel you are using." )
        <*> flag True False
              (long "no-reverse-proxy"
                <> short 'r'
                <> help "use `-r` to disable reverse proxying." )
