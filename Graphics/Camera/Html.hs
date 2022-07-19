module Graphics.Camera.Html where

import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.Time as T {- time -}
import qualified System.Directory as D {- directory -}
import System.FilePath {- filepath -}

import qualified Text.Html.Minus as H {- html-minimalist -}

import qualified Graphics.Camera.Exif as E
import qualified Graphics.Camera.Resize as R

-- * Util/Prelude

-- > collate "test" == [('e',1),('s',1),('t',2)]
collate :: Ord a => [a] -> [(a,Int)]
collate = map (\x -> (head x,length x)) . group . sort

eq_by :: Eq b => (a -> b) -> a -> a -> Bool
eq_by f p q = f p == f q

-- * Util/Time

day_to_year :: T.Day -> Int
day_to_year = fromIntegral . (\(y,_,_) -> y) . T.toGregorian

day_to_month :: T.Day -> Int
day_to_month = fromIntegral . (\(_,d,_) -> d) . T.toGregorian

-- * Util/Html

html_en :: [H.Content] -> H.Element
html_en = H.html [H.lang "en"]

-- * Img

data Img = Img {img_file_name :: FilePath
               ,img_time :: T.UTCTime
               ,img_exif_data :: [E.Exif_Tag]}
           deriving (Show)

img_date :: Img -> T.Day
img_date = T.utctDay . img_time

img_type :: Img -> String
img_type = takeExtension . img_file_name

by_year :: [Img] -> [[Img]]
by_year = groupBy (eq_by (day_to_year . img_date))

by_month :: [Img] -> [[Img]]
by_month = groupBy (eq_by (day_to_month . img_date))

-- * Html

exif_of_interest :: [E.Exif_Key]
exif_of_interest =
    ["Model"
    ,"DateTime","DateTimeOriginal"
    ,"ExposureTime","ExposureIndex","ExposureBiasValue"
    ,"FNumber","FocalLength"
    ,"ShutterSpeedValue"
    ,"ApertureValue","MaxApertureValue"
    ,"ISOSpeedRatings"
    ,"SubjectDistance"
    ,"MeteringMode"
    ]

mp4_of_interest :: [E.Exif_Key]
mp4_of_interest =
    ["CreateDate"
    ,"Duration"
    ,"ImageSize"
    ,"VideoFrameRate"
    ,"Rotation"
    ,"MIMEType"]

mk_exif :: [E.Exif_Tag] -> H.Content
mk_exif xs =
    let oi = exif_of_interest ++ mp4_of_interest
        ys = filter (\(k,_) -> k `elem` oi) xs
        f (k,v) = H.li [] [H.cdata k, H.cdata ": ", H.cdata v]
    in H.ul_c "exif" (map f ys)

up :: FilePath -> FilePath
up f = if isAbsolute f then f else "../../../" </> f

mk_node :: Int -> Img -> H.Content
mk_node n (Img fn _ xs) =
    let r_fn = R.revised_name n fn
        r_fn' = case takeExtension fn of
                  ".mp4" -> "p" </> replaceExtension r_fn "jpg"
                  _ -> r_fn
    in H.div_c "node"
        [H.div_c "image" [H.a [H.href (up fn)] [H.img [H.src (up r_fn')]]]
        ,H.div_c "text" [mk_exif xs]]

css_fn :: FilePath
css_fn = "/home/rohan/sw/hcamera/data/css/hcamera.css"

mk_page :: Int -> [Img] -> String
mk_page n xs =
    let hd = H.head [] [H.link_css "all" css_fn]
        bd = H.body_c "hcamera" [H.div_c "main" (map (mk_node n) xs)]
    in H.renderHtml5_pp (html_en [hd, bd])

write_page :: Int -> [Img] -> IO ()
write_page n img =
    case img of
      [] -> undefined
      i:is -> do let ts = img_date i
                     y = day_to_year ts
                     m = day_to_month ts
                     d = "html" </> show y </> show m -- m = two digits...
                 D.createDirectoryIfMissing True d
                 writeFile (d </> "index.html") (mk_page n (i:is))

mk_index :: [Img] -> String
mk_index xs =
    let ds = map img_date xs
        us = collate (map (\d -> (day_to_year d, day_to_month d)) ds)
        hr ((y,m),_) = show y </> show m </> "index.html"
        ft ((y,m),_) = T.formatTime
                       T.defaultTimeLocale
                       "%B, %Y"
                       (T.fromGregorian (fromIntegral y) m 0)
        nm (_,n) = " (" ++ show n ++ ")"
        ln d = H.li [] [H.a [H.href (hr d)] [H.cdata (ft d ++ nm d)]]
        hd = H.head [] [H.link_css "all" css_fn]
        bd = H.body_c "hcamera" [H.div_c "main" [H.ul [] (map ln us)]]
    in H.renderHtml5_pp (html_en [hd, bd])

write_index :: [Img] -> IO ()
write_index xs = writeFile ("html/index.html") (mk_index xs)

gen_html_tz :: T.TimeZone -> Int -> [FilePath] -> IO ()
gen_html_tz z n fn_seq = do
  print ("reading tags",length fn_seq)
  x <- mapM E.exif_read_all_tags fn_seq
  let t = map (E.exif_time_def z) x
      is = sortBy (compare `on` img_time) (zipWith3 Img fn_seq t x)
      ys = by_year is
      ms = concatMap by_month ys
  print (show ("gen_html",length fn_seq,length is,length ys,map length ms))
  write_index is
  mapM_ (write_page n) ms

gen_html :: Int -> [FilePath] -> IO ()
gen_html = gen_html_tz T.utc
