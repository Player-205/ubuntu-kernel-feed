{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as TIO

import Options
import BuildFeed
import Data.Text.Lazy.IO qualified as TIO.Lazy

main :: IO ()
main =  do
  let opts = Options { minVersion ="v5.13", minDebs = "v5.14", minChanges = "v5.15.8", noRC = True }
  f <- feed opts
  TIO.Lazy.writeFile "feed.xml" (renderFeed f)
  


