{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kernel where

import qualified Data.Text as T
import Network.HTTP.Directory
    ( (+/+), httpDirectory', httpLastModified', noTrailingSlash ) 
import Network.HTTP.Simple
    ( parseRequest_, getResponseBody, httpBS )
import Data.Foldable ( Foldable(fold) )
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe ( mapMaybe )

import Version
    ( Version(..),
      Time,
      Changes,
      Deb,
      parseVersion,
      parseChanges,
      parseTime )
import Options ( Options(..) )
import Data.Time (getCurrentTime)
import Data.List (sort)



data Kernel = Kernel 
  { version :: Version
  , debs :: Maybe [Deb]
  , changes :: Maybe Changes
  , time :: Time 
  } 

kernelPpa :: String
kernelPpa = "https://kernel.ubuntu.com/~kernel-ppa/mainline/"

kernelList :: Options ->  IO [Version]
kernelList Options{..} 
  = sort 
  . filter required 
  . mapMaybe (parseVersion . noTrailingSlash) 
  <$> httpDirectory' kernelPpa
  where
    required v = v >= minVersion && (not noRC || not (isRC v))
    isRC (Version _ _ []) = False
    isRC _ = True


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
  let link = kernelPpa <> show ver <> "/CHECKSUMS"
  maybeTime <- httpLastModified' link
  case maybeTime of
    Nothing -> parseTime <$> getCurrentTime
    Just ut -> pure $ parseTime ut


getKernel :: Options -> Version -> IO Kernel
getKernel Options{..} version = do
  debs <- whenDefault (version >= minDebs) (listDebs version)
  changes <- whenDefault (version >= minChanges) (fetchChangelog version)
  time <- getTime version
  pure Kernel{..}
 where
   whenDefault True x = Just <$> x
   whenDefault  _ _ = pure Nothing


listKernels :: Options -> IO [Kernel]
listKernels opts = do
  versions <- kernelList opts
  traverse (getKernel opts) versions
