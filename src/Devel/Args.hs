{-|
Module      : Devel.Args
Description : For handling command line arguments.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Devel.Args where

import Options.Applicative

-- | Command line arguments for yesod devel.
-- All arguments are optional.
data CmdArgs = CmdArgs
  { interfaceFile :: Maybe FilePath
  , buildFile :: Maybe FilePath
  , runFunction :: Maybe String

  -- By deafult reverse proxy should be True
  , isReverseProxy :: Bool 
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
        <$> optional (strOption
             (long "path"
               <> short 'p'
               <> metavar "FILEPATH"
               <> help "The file with the function you want to run. Default is `Application.hs`."))
        <*> optional (strOption
             (long "function"
               <> short 'f'
               <> metavar "FUNCTION"
               <> help "The function you want run. Default is `develMain`."))
        <*> optional (strOption
             (long "show-iface"
               <> metavar "FILE"
               <> help "literally the same as `ghc --show-iface`" ))
        <*> flag True False
              (long "no-reverse-proxy"
                <> short 'r'
                <> help "use `-r` to disable reverse proxying." )
