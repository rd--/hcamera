import Graphics.Camera.HTML
import Graphics.Camera.Rename
import Graphics.Camera.Resize
import System.Environment
import System.Directory {- directory -}
import System.FilePath {- filepath -}

-- > dir_subset [".cabal"] "/home/rohan/sw/hcamera"
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = do
  c <- getDirectoryContents dir
  return (map (dir </>) (filter (\nm -> takeExtension nm `elem` ext) c))

rename_dir :: [String] -> FilePath -> IO ()
rename_dir e d = dir_subset e d >>= mapM_ rename

resize_dir :: [String] -> FilePath -> IO ()
resize_dir e d = dir_subset e d >>= mapM_ (resize (Right 200))

gen_html_dir :: [String] -> FilePath -> IO ()
gen_html_dir e d = dir_subset e d >>= gen_html d

hcamera_ext :: [String]
hcamera_ext = [".jpg",".jpeg"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    [d] -> do rename_dir hcamera_ext d
              resize_dir hcamera_ext d
              gen_html_dir hcamera_ext d
    _ -> error "hcamera"
