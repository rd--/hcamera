module Graphics.Camera.Exif where

import Control.Monad
import Data.Maybe
import Data.Time {- time -}
import qualified Graphics.Exif as E {- exif -}

type Exif_Tag = (String,String)
type Exif_Tags = [Exif_Tag]

-- | Format string for Exif date, @2008:02:23 12:10:46@.
exif_date_fmt :: String
exif_date_fmt = "%Y:%m:%d %H:%M:%S"

-- | Parse Exif time.
--
-- > prs_time "2008:02:23 12:10:46"
prs_time :: String -> Maybe UTCTime
prs_time = parseTimeM True defaultTimeLocale exif_date_fmt

-- | Parse Exif date.
--
-- > prs_time_day "2008:02:23 12:10:46"
prs_time_day :: String -> Maybe Day
prs_time_day = parseTimeM True defaultTimeLocale exif_date_fmt

-- | Format @UTCTime@ in manner suitable for use as a filename.
--
-- > fmt_time (prs_time "2008:02:23 12:10:46") == "2008-02-23-12-10-46"
fmt_time :: UTCTime -> String
fmt_time = formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

-- | Read 'Exif_Tags' from file.
read_all_tags :: FilePath -> IO Exif_Tags
read_all_tags f = do
  x <- E.fromFile f
  E.allTags x

exif_filter :: Exif_Tags -> [String] -> Exif_Tags
exif_filter e t = filter (flip elem t . fst) e

exif_datetime_seq :: Exif_Tags -> Exif_Tags
exif_datetime_seq e =
    exif_filter e ["DateTime"
                  ,"DateTimeOriginal"
                  ,"DateTimeDigitized"]

exif_datetime :: Exif_Tags -> Maybe Exif_Tag
exif_datetime e =
    case exif_datetime_seq e of
      t:_ -> Just t
      _ -> Nothing

exif_time :: Exif_Tags -> Maybe UTCTime
exif_time = join . fmap (prs_time . snd) . exif_datetime

exif_day :: Exif_Tags -> Maybe Day
exif_day = join . fmap (prs_time_day . snd) . exif_datetime

exif_day_def :: Exif_Tags -> Day
exif_day_def = fromMaybe (fromGregorian 1970 0 0) . exif_day
