-- * Date/Time
module Graphics.Camera.Time where

import qualified Data.Time as T {- time -}

difftime_to_nominaldifftime :: T.DiffTime -> T.NominalDiffTime
difftime_to_nominaldifftime = realToFrac

seconds_to_nominaldifftime :: Integer -> T.NominalDiffTime
seconds_to_nominaldifftime = difftime_to_nominaldifftime . T.secondsToDiffTime

timezone_to_seconds :: Integral i => T.TimeZone -> i
timezone_to_seconds = (* 60) . fromIntegral . T.timeZoneMinutes

-- | 'T.Timezone' as 'T.NominalDiffTime'.
--
-- > z <- T.getCurrentTimeZone
-- > T.timeZoneName z == "AEDT"
-- > T.timeZoneMinutes z == 660
-- > T.timeZoneOffsetString z == "+1100"
-- > timezone_to_seconds z == 39600
-- > timezone_to_nominaldifftime z == 39600
timezone_to_nominaldifftime :: T.TimeZone -> T.NominalDiffTime
timezone_to_nominaldifftime = seconds_to_nominaldifftime . timezone_to_seconds

-- | Parse 'T.TimeZone', or error.  Names recognised are those in RFC-822.
--
-- > timezone_to_seconds (timezone_parse "UT") == 0
-- > timezone_to_seconds (timezone_parse "EST") == -18000
-- > timezone_to_seconds (timezone_parse "+11:00") == 39600
timezone_parse :: String -> T.TimeZone
timezone_parse = T.parseTimeOrError True T.defaultTimeLocale "%Z"

time_shift_by_timezone :: T.TimeZone -> T.UTCTime -> T.UTCTime
time_shift_by_timezone z t = T.addUTCTime (timezone_to_nominaldifftime z) t

-- > let Just t = exif_parse_time "2017:02:05 09:43:31"
-- > time_shift_by_current_timezone t
time_shift_by_current_timezone :: T.UTCTime -> IO T.UTCTime
time_shift_by_current_timezone t = do
  z <- T.getCurrentTimeZone
  return (time_shift_by_timezone z t)

