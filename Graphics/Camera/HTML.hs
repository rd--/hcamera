module Graphics.Camera.HTML where

import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.Time as T {- time -}
import qualified System.Directory as D {- directory -}
import System.FilePath {- filepath -}

import qualified Text.XML.Light as X {- xml -}

import qualified Text.HTML.Minus as H {- html-minimalist -}

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

-- * Util/HTML

html_en :: [X.Content] -> X.Element
html_en = H.html [H.lang "en"]

body_c :: String -> [X.Content] -> X.Content
body_c c = H.body [H.class' c]

div_c :: String -> [X.Content] -> X.Content
div_c c = H.div [H.class' c]

ul_c :: String -> [X.Content] -> X.Content
ul_c c = H.ul [H.class' c]

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

-- * HTML

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

mk_exif :: [E.Exif_Tag] -> X.Content
mk_exif xs =
    let oi = exif_of_interest ++ mp4_of_interest
        ys = filter (\(k,_) -> k `elem` oi) xs
        f (k,v) = H.li [] [H.cdata k, H.cdata ": ", H.cdata v]
    in ul_c "exif" (map f ys)

up :: FilePath -> FilePath
up f = if isAbsolute f then f else "../../../" </> f

mk_node :: Img -> X.Content
mk_node (Img fn _ xs) =
    let fn' = replaceExtension (R.revised_name fn) "jpg"
    in div_c "node"
        [div_c "image" [H.a [H.href (up fn)] [H.img [H.src (up fn')]]]
        ,div_c "text" [mk_exif xs]]

css_fn :: FilePath
css_fn = "/home/rohan/sw/hcamera/data/css/hcamera.css"

mk_page :: [Img] -> String
mk_page xs =
    let hd = H.head [] [H.link_css "all" css_fn]
        bd = body_c "hcamera" [div_c "main" (map mk_node xs)]
    in H.renderHTML5 (html_en [hd, bd])

write_page :: FilePath -> [Img] -> IO ()
write_page dir img =
    case img of
      [] -> undefined
      i:is -> do let ts = img_date i
                     y = day_to_year ts
                     m = day_to_month ts
                     d = dir </> "html" </> show y </> show m -- m = two digits...
                 D.createDirectoryIfMissing True d
                 writeFile (d </> "index.html") (mk_page (i:is))

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
        bd = body_c "hcamera" [div_c "main" [H.ul [] (map ln us)]]
    in H.renderHTML5 (html_en [hd, bd])

write_index :: FilePath -> [Img] -> IO ()
write_index dir xs = writeFile (dir </> "html/index.html") (mk_index xs)

gen_html :: T.TimeZone -> FilePath -> [FilePath] -> IO ()
gen_html z dir f = do
  print ("reading tags",dir,length f)
  x <- mapM E.exif_read_all_tags f
  let t = map (E.exif_time_def z) x
      is = sortBy (compare `on` img_time) (zipWith3 Img f t x)
      ys = by_year is
      ms = concatMap by_month ys
  print (show ("gen_html",dir,length f,length is,length ys,map length ms))
  write_index dir is
  mapM_ (write_page dir) ms
