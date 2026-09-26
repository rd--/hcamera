-- | Html
module Graphics.Camera.Html where

import qualified Data.Function {- base -}
import qualified Data.List {- base -}

import qualified Data.Time {- time -}
import qualified System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Text.Html.Minus as Html {- html-minimalist -}

import qualified Graphics.Camera.Exif as Exif
import qualified Graphics.Camera.Resize as Resize

-- * Util/Prelude

{- | Collate

>>> collate "test"
[('e',1),('s',1),('t',2)]
-}
collate :: Ord a => [a] -> [(a, Int)]
collate =
  map (\x -> (head x, length x))
    . Data.List.group
    . Data.List.sort

eq_by :: Eq b => (a -> b) -> a -> a -> Bool
eq_by f p q = f p == f q

-- * Util/Time

day_to_year :: Data.Time.Day -> Int
day_to_year = fromIntegral . (\(y, _, _) -> y) . Data.Time.toGregorian

day_to_month :: Data.Time.Day -> Int
day_to_month = fromIntegral . (\(_, d, _) -> d) . Data.Time.toGregorian

-- * Util/Html

html_en :: [Html.Content] -> Html.Element
html_en = Html.html [Html.lang "en"]

-- * Img

data Img = Img
  { img_file_name :: FilePath
  , img_time :: Data.Time.UTCTime
  , img_exif_data :: [Exif.Exif_Tag]
  }
  deriving (Show)

img_date :: Img -> Data.Time.Day
img_date = Data.Time.utctDay . img_time

img_type :: Img -> String
img_type = takeExtension . img_file_name

by_year :: [Img] -> [[Img]]
by_year = Data.List.groupBy (eq_by (day_to_year . img_date))

by_month :: [Img] -> [[Img]]
by_month = Data.List.groupBy (eq_by (day_to_month . img_date))

-- * Html

exif_of_interest :: [Exif.Exif_Key]
exif_of_interest =
  [ "Model"
  , "DateTime"
  , "DateTimeOriginal"
  , "ExposureTime"
  , "ExposureIndex"
  , "ExposureBiasValue"
  , "FNumber"
  , "FocalLength"
  , "ShutterSpeedValue"
  , "ApertureValue"
  , "MaxApertureValue"
  , "ISOSpeedRatings"
  , "SubjectDistance"
  , "MeteringMode"
  ]

mp4_of_interest :: [Exif.Exif_Key]
mp4_of_interest =
  [ "CreateDate"
  , "Duration"
  , "ImageSize"
  , "VideoFrameRate"
  , "Rotation"
  , "MIMEType"
  ]

mk_exif :: [Exif.Exif_Tag] -> Html.Content
mk_exif xs =
  let oi = exif_of_interest ++ mp4_of_interest
      ys = filter (\(k, _) -> k `elem` oi) xs
      f (k, v) = Html.li [] [Html.cdata k, Html.cdata ": ", Html.cdata v]
  in Html.ul_c "exif" (map f ys)

up :: FilePath -> FilePath
up f = if isAbsolute f then f else "../../../" </> f

mk_node :: Int -> Img -> Html.Content
mk_node n (Img fn _ xs) =
  let r_fn = Resize.revised_name n fn
      r_fn' = case takeExtension fn of
        ".mp4" -> "p" </> replaceExtension r_fn "jpg"
        _ -> r_fn
  in Html.div_c
      "node"
      [ Html.div_c "image" [Html.a [Html.href (up fn)] [Html.img [Html.src (up r_fn')]]]
      , Html.div_c "text" [mk_exif xs]
      ]

css_fn :: FilePath
css_fn = "/home/rohan/sw/hcamera/data/css/hcamera.css"

mk_page :: Int -> [Img] -> String
mk_page n xs =
  let hd = Html.head [] [Html.link_css "all" css_fn]
      bd = Html.body_c "hcamera" [Html.div_c "main" (map (mk_node n) xs)]
  in Html.renderHtml5_pp (html_en [hd, bd])

write_page :: Int -> [Img] -> IO ()
write_page n img =
  case img of
    [] -> undefined
    i : is -> do
      let ts = img_date i
          y = day_to_year ts
          m = day_to_month ts
          d = "html" </> show y </> show m -- m = two digits...
      System.Directory.createDirectoryIfMissing True d
      writeFile (d </> "index.html") (mk_page n (i : is))

mk_index :: [Img] -> String
mk_index xs =
  let ds = map img_date xs
      us = collate (map (\d -> (day_to_year d, day_to_month d)) ds)
      hr ((y, m), _) = show y </> show m </> "index.html"
      ft ((y, m), _) =
        Data.Time.formatTime
          Data.Time.defaultTimeLocale
          "%B, %Y"
          (Data.Time.fromGregorian (fromIntegral y) m 0)
      nm (_, n) = " (" ++ show n ++ ")"
      ln d = Html.li [] [Html.a [Html.href (hr d)] [Html.cdata (ft d ++ nm d)]]
      hd = Html.head [] [Html.link_css "all" css_fn]
      bd = Html.body_c "hcamera" [Html.div_c "main" [Html.ul [] (map ln us)]]
  in Html.renderHtml5_pp (html_en [hd, bd])

write_index :: [Img] -> IO ()
write_index xs = writeFile ("html/index.html") (mk_index xs)

gen_html_tz :: Data.Time.TimeZone -> Int -> [FilePath] -> IO ()
gen_html_tz z n fn_seq = do
  print ("reading tags", length fn_seq)
  x <- mapM Exif.exif_read_all_tags fn_seq
  let t = map (Exif.exif_time_def z) x
      is = Data.List.sortBy (compare `Data.Function.on` img_time) (zipWith3 Img fn_seq t x)
      ys = by_year is
      ms = concatMap by_month ys
  print (show ("gen_html", length fn_seq, length is, length ys, map length ms))
  write_index is
  mapM_ (write_page n) ms

gen_html :: Int -> [FilePath] -> IO ()
gen_html = gen_html_tz Data.Time.utc
