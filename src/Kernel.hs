{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel where

import qualified Data.Text as T
import Network.HTTP.Directory 
import Network.HTTP.Simple
import Data.Foldable
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe ( mapMaybe )

import Version
import Options
import Data.Time (getCurrentTime)



data Kernel = Kernel 
  { version :: Version
  , debs :: Maybe [Deb]
  , changes :: Maybe Changes
  , time :: Time 
  } 

kernelPpa :: String
kernelPpa = "https://kernel.ubuntu.com/~kernel-ppa/mainline/"

kernelList :: IO [Version]
kernelList = mapMaybe (parseVersion . noTrailingSlash) <$> httpDirectory' kernelPpa

fetchChangelog :: Version -> IO Changes
fetchChangelog ver = do
  let req = parseRequest_ (kernelPpa <> show ver <> "/CHANGES")
  body <- getResponseBody <$> httpBS req
  pure $ parseChanges body

listDebs :: Version -> IO [Deb]
listDebs ver = do
  let link = kernelPpa <> show ver <> "/"
  content <- httpDirectory' link
  let dirs = filter (T.isSuffixOf "/") content
  nestedContent <- fold <$> traverse (httpDirectory' . (link +/+) . T.unpack) dirs
  pure . map (T.pack link <>) . filter requiered $ (content <> nestedContent)
  where
    requiered t = isAllDeb t || (isGeneric t && isAmdDeb t)
    isAllDeb = T.isSuffixOf "_all.deb"
    isAmdDeb = T.isSuffixOf "_amd64.deb"
    isGeneric = T.isInfixOf "generic"

getTime :: Version -> IO Time
getTime ver = do
  let link = kernelPpa <> show ver <> "/" <> "CHECKSUMS"
  maybeTime <- httpLastModified' link
  case maybeTime of
    Nothing -> parseTime <$> getCurrentTime
    Just ut -> pure $ parseTime ut


buildKernel :: Options -> Version -> IO Kernel
buildKernel Options{..} version = do
  debs <- whenDefault (version >= minDebs) (listDebs version)
  changes <- whenDefault (version >= minChanges) (fetchChangelog version)
  time <- getTime version
  pure Kernel{..}
 where
   whenDefault True x = Just <$> x
   whenDefault  _ _ = pure Nothing