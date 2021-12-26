{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Version where

import Data.Text qualified as T
import Data.String (IsString (fromString))
import Data.Maybe (fromJust)
import Control.Monad ((<=<))
import Control.Applicative (liftA3, Applicative (liftA2))
import Text.Read (readMaybe)
import Data.ByteString ( ByteString )
import Data.Text.Encoding qualified as T
import Data.Time ( UTCTime )
import Data.Coerce (coerce)
import Data.Text.Encoding (decodeUtf8)
import Data.Function (on)

newtype Deb = Deb T.Text

parseDeb :: T.Text -> T.Text -> Deb 
parseDeb link deb = coerce (link <> deb) 

buildDebs :: [Deb] -> T.Text
buildDebs = flip T.snoc '\n' . T.unlines . coerce

newtype KernelHeader = KernelHeader T.Text 

parseHeader :: ByteString -> KernelHeader
parseHeader = coerce . decodeUtf8

buildHeader :: KernelHeader -> T.Text
buildHeader = coerce

newtype Changes = Changes T.Text

parseChanges :: T.Text  -> Changes
parseChanges = coerce 

buildChanges :: Changes -> T.Text
buildChanges changes = "<a href=\"" <> coerce changes <> "\">CHANGES</a>\n<br>"

newtype Time = Time UTCTime

parseTime :: UTCTime -> Time
parseTime = coerce

buildTime :: Time -> T.Text
buildTime = T.pack . show @UTCTime . coerce 

newtype Suffix = Suffix [T.Text]
  deriving newtype Eq

instance Ord Suffix where
  compare (Suffix []) (Suffix []) = EQ
  compare (Suffix []) _ = GT
  compare _ (Suffix []) = LT
  compare (Suffix xs) (Suffix ys) = compare xs ys 

buildSuffix :: Suffix -> T.Text
buildSuffix = foldMap (T.cons '-') . coerce @_ @[T.Text]


isRC :: Version -> Bool
isRC (Version _ _ (Suffix [])) = False
isRC _ = True

type Minor = Int

data Major = Major Int Int
  deriving (Eq, Ord)

instance Show Major where
  show (Major x yy) = show x <> "." <> show yy

data Version = Version Major (Maybe Minor) Suffix
  deriving (Eq, Ord)

instance IsString Version where
  fromString = fromJust . parseVersion . T.pack
instance Show Version where
  show (Version major minor suffix) =
    "v"
    <> show major
    <> foldMap (('.':) . show) minor
    <> T.unpack (buildSuffix suffix)


parseVersion :: T.Text -> Maybe Version
parseVersion = buildVersion <=< fmap (T.splitOn ".") . T.stripPrefix "v"
  where
    readText = readMaybe @Int . T.unpack
    parseSuffix str =  (readText num, suff)
      where (num:suff) = T.splitOn "-" str

    buildVersion [x, yy] = liftA3 Version major (pure Nothing) (pure (coerce suff))
      where
        (ver, suff) = parseSuffix yy
        major = liftA2 Major (readText x) ver
    buildVersion [x, yy, zzz] = liftA3 Version major (pure minor) (pure (coerce suff))
      where
        (minor, suff) = parseSuffix zzz
        major = liftA2 Major (readText x) (readText yy)
    buildVersion _ = Nothing
