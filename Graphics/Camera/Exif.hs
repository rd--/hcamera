-- | EXIF utilities
module Graphics.Camera.Exif where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import qualified Data.Time as T {- time -}

import qualified Graphics.Exif as E {- exif -}

-- * Date/Time

-- | Format string for Exif date, @2008:02:23 12:10:46@.
exif_date_fmt :: String
exif_date_fmt = "%Y:%m:%d %H:%M:%S"

-- | Parse Exif time.
--
-- > exif_parse_time "2008:02:23 12:10:46"
exif_parse_time :: String -> Maybe T.UTCTime
exif_parse_time = T.parseTimeM True T.defaultTimeLocale exif_date_fmt

-- | Parse Exif date.
--
-- > exif_parse_time_day "2008:02:23 12:10:46"
exif_parse_time_day :: String -> Maybe T.Day
exif_parse_time_day = T.parseTimeM True T.defaultTimeLocale exif_date_fmt

-- | Format @UTCTime@ in manner suitable for use as a filename.
--
-- > fmap exif_format_time (exif_parse_time "2008:02:23 12:10:46") == Just "2008-02-23-12-10-46"
exif_format_time :: T.UTCTime -> String
exif_format_time = T.formatTime T.defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

-- * Exif

type Exif_Key = String

type Exif_Value = String

-- | (key,value) pair.
type Exif_Tag = (Exif_Key,Exif_Value)

exif_attr_ext :: [Exif_Key]
exif_attr_ext =
    ["Make"
    ,"XResolution","YResolution","ResolutionUnit"
    ,"PixelXDimension","PixelYDimension"
    ,"ComponentsConfiguration"
    ,"YCbCrPositioning"
    ,"Compression"
    ,"ExifVersion"
    ,"ColorSpace"
    ,"WhiteBalance"
    ,"GainControl"
    ,"InteroperabilityIndex","InteroperabilityVersion"
    ]

exif_attr :: [Exif_Key]
exif_attr =
    ["Model"
    ,"DateTime","DateTimeOriginal","DateTimeDigitized"
    ,"ExposureTime","ExposureIndex","ExposureBiasValue"
    ,"FNumber","FocalLength"
    ,"ShutterSpeedValue"
    ,"ApertureValue","MaxApertureValue"
    ,"ISOSpeedRatings"
    ,"SubjectDistance"
    ,"MeteringMode"
    ,"Flash"
    ]

-- | Read '[Exif_Tag]' from file.
exif_read_all_tags :: FilePath -> IO [Exif_Tag]
exif_read_all_tags f = do
  x <- E.fromFile f
  E.allTags x

exif_filter :: [Exif_Tag] -> [Exif_Key] -> [Exif_Tag]
exif_filter e t = filter (flip elem t . fst) e

exif_datetime_seq :: [Exif_Tag] -> [Exif_Tag]
exif_datetime_seq e =
    exif_filter e ["DateTime"
                  ,"DateTimeOriginal"
                  ,"DateTimeDigitized"]

exif_datetime :: [Exif_Tag] -> Maybe Exif_Tag
exif_datetime e =
    case exif_datetime_seq e of
      t:_ -> Just t
      _ -> Nothing

exif_time :: [Exif_Tag] -> Maybe T.UTCTime
exif_time = join . fmap (exif_parse_time . snd) . exif_datetime

exif_time_def :: [Exif_Tag] -> T.UTCTime
exif_time_def = fromMaybe (T.UTCTime (T.fromGregorian 1970 0 0) 0) . exif_time

exif_day :: [Exif_Tag] -> Maybe T.Day
exif_day = join . fmap (exif_parse_time_day . snd) . exif_datetime

exif_day_def :: [Exif_Tag] -> T.Day
exif_day_def = fromMaybe (T.fromGregorian 1970 0 0) . exif_day
