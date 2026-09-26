-- | Date & Time
module Graphics.Camera.Time where

import qualified Data.Time {- time -}

difftime_to_nominaldifftime :: Data.Time.DiffTime -> Data.Time.NominalDiffTime
difftime_to_nominaldifftime = realToFrac

seconds_to_nominaldifftime :: Integer -> Data.Time.NominalDiffTime
seconds_to_nominaldifftime = difftime_to_nominaldifftime . Data.Time.secondsToDiffTime

timezone_to_seconds :: Integral i => Data.Time.TimeZone -> i
timezone_to_seconds = (* 60) . fromIntegral . Data.Time.timeZoneMinutes

{- | 'Data.Time.Timezone' as 'Data.Time.NominalDiffTime'.

>>> let z = timezone_parse "+11:00"
>>> Data.Time.timeZoneName z
""

>>> Data.Time.timeZoneMinutes z
660

>>> Data.Time.timeZoneOffsetString z
"+1100"

>>> timezone_to_seconds z
39600

>>> timezone_to_nominaldifftime z
39600s
-}
timezone_to_nominaldifftime :: Data.Time.TimeZone -> Data.Time.NominalDiffTime
timezone_to_nominaldifftime = seconds_to_nominaldifftime . timezone_to_seconds

{- | Parse 'Data.Time.TimeZone', or error.  Names recognised are those in RFC-822.

>>> timezone_to_seconds (timezone_parse "UT")
0

>>> timezone_to_seconds (timezone_parse "EST")
-18000

>>> timezone_to_seconds (timezone_parse "+11:00")
39600
-}
timezone_parse :: String -> Data.Time.TimeZone
timezone_parse = Data.Time.parseTimeOrError True Data.Time.defaultTimeLocale "%Z"

time_shift_by_timezone :: Data.Time.TimeZone -> Data.Time.UTCTime -> Data.Time.UTCTime
time_shift_by_timezone z t = Data.Time.addUTCTime (timezone_to_nominaldifftime z) t

{- | Time shift by current time zone

> let Just t = exif_parse_time "2017:02:05 09:43:31"
> time_shift_by_current_timezone t
-}
time_shift_by_current_timezone :: Data.Time.UTCTime -> IO Data.Time.UTCTime
time_shift_by_current_timezone t = do
  z <- Data.Time.getCurrentTimeZone
  return (time_shift_by_timezone z t)
