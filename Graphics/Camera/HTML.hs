module Graphics.Camera.HTML where

import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.Time as T {- time -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Text.XML.Light as X {- xml -}

import qualified Text.HTML.Minus as H {- html-minimalist -}

import qualified Graphics.Camera.Exif as E
import qualified Graphics.Camera.Resize as R

std_html_attr :: [X.Attr]
std_html_attr = [H.lang "en" ]

std_meta :: String -> String -> [X.Content]
std_meta d s =
    [H.title [] [H.cdata ("hcamera: " ++ d)]
    ,H.meta_description d
    ,H.meta_author "hcamera"
    ,H.link_css "all" s]

data Img = Img {file_name :: FilePath
               ,date :: T.Day
               ,time :: T.UTCTime
               ,exif_data :: [E.Exif_Tag]}
           deriving (Show)

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

mk_exif :: [E.Exif_Tag] -> X.Content
mk_exif xs =
    let ys = filter (\(k,_) -> k `elem` exif_of_interest) xs
        f (k,v) = H.li [] [H.cdata k, H.cdata ": ", H.cdata v]
    in H.ul [H.class' "exif"] (map f ys)

up :: FilePath -> FilePath
up f = if isAbsolute f then f else "../../../" </> f

mk_node :: Img -> X.Content
mk_node (Img f _ _ xs) =
    H.div
         [H.class' "node"]
         [H.div [H.class' "image"] [H.a [H.href (up f)] [H.img [H.src (up (R.revised_name f))]]]
         ,H.div [H.class' "text"] [mk_exif xs]]

mk_page :: [Img] -> String
mk_page xs =
    let e = H.html std_html_attr [hd, bd]
        hd = H.head [] (std_meta "hcamera" (up "css/hcamera.css"))
        bd = H.body
             [H.class' "hcamera"]
             [H.div
              [H.class' "main"]
              (map mk_node xs)]
    in H.renderHTML5 e

day_year :: T.Day -> Int
day_year = fromIntegral . (\(y,_,_) -> y) . T.toGregorian

day_month :: T.Day -> Int
day_month = fromIntegral . (\(_,d,_) -> d) . T.toGregorian

eq_by :: Eq b => (a -> b) -> a -> a -> Bool
eq_by f p q = f p == f q

by_year :: [Img] -> [[Img]]
by_year is = groupBy (eq_by (day_year . date)) is

by_month :: [Img] -> [[Img]]
by_month is = groupBy (eq_by (day_month . date)) is

write_page :: FilePath -> [Img] -> IO ()
write_page dir img =
    case img of
      [] -> undefined
      i:is -> do let ts = date i
                     y = day_year ts
                     m = day_month ts
                     d = dir </> "html" </> show y </> show m
                 createDirectoryIfMissing True d
                 writeFile (d </> "index.html") (mk_page (i:is))

-- > collate "test" == [('e',1),('s',1),('t',2)]
collate :: Ord a => [a] -> [(a,Int)]
collate = map (\x -> (head x,length x)) . group . sort

mk_index :: [Img] -> String
mk_index xs =
    let ds = map date xs
        us = collate (map (\d -> (day_year d, day_month d)) ds)
        hr ((y,m),_) = show y </> show m </> "index.html"
        ft ((y,m),_) = T.formatTime
                       T.defaultTimeLocale
                       "%B, %Y"
                       (T.fromGregorian (fromIntegral y) m 0)
        nm (_,n) = " (" ++ show n ++ ")"
        ln d = H.li [] [H.a [H.href (hr d)] [H.cdata (ft d ++ nm d)]]
        e = H.html std_html_attr [hd, bd]
        hd = H.head [] (std_meta "hcamera" "../css/hcamera.css")
        bd = H.body
             [H.class' "hcamera"]
             [H.div
              [H.class' "main"]
              [H.ul [] (map ln us)]]
    in H.renderXHTML H.xhtml_1_0_strict e

write_index :: FilePath -> [Img] -> IO ()
write_index dir xs = writeFile (dir </> "html/index.html") (mk_index xs)

gen_html :: FilePath -> [FilePath] -> IO ()
gen_html dir f = do
  print ("reading tags",dir,length f)
  x <- mapM E.exif_read_all_tags f
  let d = map E.exif_day_def x
      t = map E.exif_time_def x
      is = sortBy (compare `on` time) (zipWith4 Img f d t x)
      ys = by_year is
      ms = concatMap by_month ys
  print (show ("gen_html",dir,length f,length is,length ys,map length ms))
  write_index dir is
  mapM_ (write_page dir) ms
