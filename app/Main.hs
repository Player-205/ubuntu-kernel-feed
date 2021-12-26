{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as TIO

import Options
import BuildFeed
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy.Char8 qualified as BS

main :: IO ()
main =  opts >>= \opts -> do
  f <- feed opts
  let bs = encodeUtf8 (renderFeed f)
  BS.writeFile (fileName opts) bs
  


