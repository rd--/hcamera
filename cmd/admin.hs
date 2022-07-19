import System.Environment {- base -}

import qualified Graphics.Camera.Exif as Exif
import qualified Graphics.Camera.Html as Html
import qualified Graphics.Camera.Rename as Rename
import qualified Graphics.Camera.Resize as Resize

{-
import System.Directory {- directory -}
import System.FilePath {- filepath -}

-- > dir_subset [".cabal"] "/home/rohan/sw/hcamera"
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = do
  c <- getDirectoryContents dir
  return (map (dir </>) (filter (\nm -> takeExtension nm `elem` ext) c))
-}

exif_print_libexif :: [FilePath] -> IO ()
exif_print_libexif = mapM_ (\fn -> Exif.libexif_read_all_tags fn >>= print)

exif_print_meta :: [FilePath] -> IO ()
exif_print_meta = mapM_ (\fn -> Exif.meta_read_all_tags fn >>= print)

exif_print :: [FilePath] -> IO ()
exif_print = mapM_ (\fn -> Exif.exif_read_all_tags fn >>= print)

html_gen :: [FilePath] -> IO ()
html_gen fn_set = Html.gen_html 200 fn_set

exif_rename :: [FilePath] -> IO ()
exif_rename = mapM_ Rename.rename

img_resize :: [FilePath] -> IO ()
img_resize = mapM_ (Resize.resize (Right 200))

help :: [String]
help =
    ["hcamera-admin cmd file-name..."
    ,""
    ,"  exif-print | exif-print-libexif | exif-print-meta"
    ,"  exif-rename"
    ,"  html-gen"
    ,"  img-resize"
    ]

usage :: IO ()
usage = putStrLn (unlines help)

main :: IO ()
main = do
  a <- getArgs
  case a of
    "exif-print":fn -> exif_print fn
    "exif-print-libexif":fn -> exif_print_libexif fn
    "exif-print-meta":fn -> exif_print_meta fn
    "exif-rename":fn -> exif_rename fn
    "html-gen":fn -> html_gen fn
    "img-resize":fn -> img_resize fn
    _ -> usage
