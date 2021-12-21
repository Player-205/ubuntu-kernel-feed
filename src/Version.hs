{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Version where
import qualified Data.Text as T
import Data.String (IsString (fromString))
import Data.Maybe (fromJust)
import Control.Monad ((<=<))
import Control.Applicative (liftA3, Applicative (liftA2))
import Text.Read (readMaybe)
import Data.Ord
import Data.Foldable
import Data.ByteString
import qualified Data.Text.Encoding as T
import Data.Time

type Deb = T.Text

buildDebs :: [Deb] -> T.Text 
buildDebs = T.unlines 

type Changes = T.Text 

parseChanges :: ByteString -> Changes
parseChanges = T.decodeUtf8

buildChanges :: Changes -> T.Text
buildChanges = id

type Time = T.Text

parseTime :: UTCTime -> Time
parseTime = T.pack . show 

buildTime :: Time -> T.Text
buildTime = id

type Suffix = [T.Text]

buildSuffix :: Suffix -> T.Text 
buildSuffix = foldMap ("-"<>) 


type Minor = Int

data Major = Major Int Int
  deriving (Eq, Ord)

instance Show Major where
  show (Major x yy) = show x <> "." <> show yy

data Version = Version Major (Maybe Minor) Suffix
  deriving (Eq)

instance Ord Version where
  compare (Version x yy suff) (Version x' yy' suff') =
    compare x x' `andThen` compare yy yy' `andThen`
      case (suff, suff') of
        ([], []) -> EQ
        ([], _) -> GT     
        (_, []) -> LT     
        _ -> compare suff suff'  
    where
      andThen EQ y = y
      andThen x _ = x

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

    buildVersion [x, yy] = liftA3 Version major (pure Nothing) (pure suff)
      where
        (ver, suff) = parseSuffix yy
        major = liftA2 Major (readText x) ver
    buildVersion [x, yy, zzz] = liftA3 Version major (pure minor) (pure suff)
      where
        (minor, suff) = parseSuffix zzz
        major = liftA2 Major (readText x) (readText yy)
    buildVersion _ = Nothing
