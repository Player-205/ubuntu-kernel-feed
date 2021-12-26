{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Options where




import Version

import Options.Applicative
import Options.Applicative.Types

import qualified Data.Text as T

data Options = Options 
  { minVersion :: Version
  , fileName :: String
  , noRC :: Bool
  }


opts :: IO Options
opts = execParser optsInfo
  where
    optsInfo ::ParserInfo Options
    optsInfo = info (helper <*> liftA3 Options version file rc) 
      (fullDesc
      <> header "ubuntu-kernel-feed - feed-generator for ubuntu-ppa" ) 


version :: Parser Version
version = flip BindP (maybe empty pure) 
  $ parseVersion . T.cons 'v' <$> strOption
  (long "min-version"
  <> short 'v'
  <> help "Minimal kernel version. Format: x.yy[.zzz][-suffix]")

file :: Parser String
file = strOption
  (long "output"
  <> short 'o'
  <> value "feed.xml"
  <> help "Name of output file. Default: feed.xml")

rc :: Parser Bool
rc = switch
  (long "no-rc"
  <> help "Delete versions with suffix from output.")

