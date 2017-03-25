import System.Environment {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Graphics.Camera.Exif as Exif
import qualified Graphics.Camera.HTML as HTML
import qualified Graphics.Camera.Rename as Rename
import qualified Graphics.Camera.Resize as Resize

-- > dir_subset [".cabal"] "/home/rohan/sw/hcamera"
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = do
  c <- getDirectoryContents dir
  return (map (dir </>) (filter (\nm -> takeExtension nm `elem` ext) c))

exif_print :: [String] -> FilePath -> IO ()
exif_print e d = dir_subset e d >>= mapM_ (\fn -> Exif.exif_read_all_tags fn >>= print)

html_gen :: [String] -> FilePath -> IO ()
html_gen e d = dir_subset e d >>= HTML.gen_html "."

img_rename :: [String] -> FilePath -> IO ()
img_rename e d = dir_subset e d >>= mapM_ Rename.rename

img_resize :: [String] -> FilePath -> IO ()
img_resize e d = dir_subset e d >>= mapM_ (Resize.resize (Right 200))

help :: [String]
help =
    ["hcamera-admin cmd dir"
    ,""
    ,"cmd = exif-print html-gen img-rename img-resize"]

usage :: IO ()
usage = putStrLn (unlines help)

main :: IO ()
main = do
  a <- getArgs
  let ext = [".jpg",".jpeg",".JPG"]
  case a of
    [c,d] -> case c of
               "exif-print" -> exif_print ext d
               "html-gen" -> html_gen ext d
               "img-rename" -> img_rename ext d
               "img-resize" -> img_resize ext d
               _ -> usage
    _ -> usage
