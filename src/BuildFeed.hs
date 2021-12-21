{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module BuildFeed where

import Kernel ( kernelPpa, Kernel(..), listKernels )
import Version ( buildDebs, buildChanges, buildTime, parseTime )

import Text.Feed.Types ( Feed(AtomFeed) )
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Export (textFeedWith)
import Text.XML (def, rsPretty)
import Data.Text.Lazy qualified as Lazy
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Options ( Options )
import Data.Time (getCurrentTime)


renderFeed :: Atom.Feed -> Lazy.Text
renderFeed = fromJust . Export.textFeedWith def{rsPretty = True} . AtomFeed


toEntry :: Kernel -> Atom.Entry
toEntry Kernel{..} =
  (Atom.nullEntry
  (Text.pack $ kernelPpa <> show version)
  (Atom.TextString (Text.pack $ show version))
  (buildTime time))
  { Atom.entryContent = Atom.TextContent <$> fmap buildDebs debs <> fmap buildChanges changes }


feed :: Options -> IO Atom.Feed
feed opts = do
  kernels <- listKernels opts
  time <- buildTime  . parseTime <$> getCurrentTime
  pure (Atom.nullFeed
       (Text.pack kernelPpa)
       (Atom.TextString "")
       time) { Atom.feedEntries =  map toEntry kernels }