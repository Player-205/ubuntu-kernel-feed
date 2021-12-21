{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module BuildFeed where

import Kernel
import Version

import Text.Feed.Types
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)
import Text.XML (def, rsPretty)
import qualified Data.Text.Lazy as Lazy
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Options
import Data.Time (getCurrentTime)
import Data.List (sort)


renderFeed :: Atom.Feed -> Lazy.Text
renderFeed = fromJust . Export.textFeedWith def{rsPretty = True} . AtomFeed


fetchEntries :: Options -> IO [Atom.Entry]
fetchEntries opts@Options{..} = do
  let pred = \v -> v >= minVersion && (not noRC || not (isRC v))

  versions <- sort . filter pred <$> kernelList
  kernels <- traverse (buildKernel opts) versions
  pure $ map toEntry kernels
  where
    isRC (Version _ _ []) = False
    isRC _ = True



toEntry :: Kernel -> Atom.Entry
toEntry Kernel{..} =
  (Atom.nullEntry
  (Text.pack $ kernelPpa <> show version)
  (Atom.TextString (Text.pack $ show version))
  (buildTime time))
  { Atom.entryContent = Atom.TextContent <$> fmap buildDebs debs <> fmap buildChanges changes }


feed :: Options -> IO Atom.Feed
feed opts = do
  entries <- fetchEntries opts
  time <- buildTime  . parseTime <$> getCurrentTime
  pure (Atom.nullFeed
       (Text.pack kernelPpa)
       (Atom.TextString "")
       time) { Atom.feedEntries = entries }