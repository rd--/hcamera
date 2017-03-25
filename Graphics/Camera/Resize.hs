module Graphics.Camera.Resize where

import Control.Monad {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import System.Process {- process -}

type Command = (String,[String])

-- > resize_cmd (Right 200) "x.jpg" "n/x.jpg"
resize_cmd :: Either Int Int -> FilePath -> FilePath -> Command
resize_cmd dm ifn ofn =
    let dm' = case dm of
                Left w -> show w ++ "x"
                Right h -> "x" ++ show h
    in ("convert",["-resize",dm',"-colorspace","rgb",ifn,ofn])

-- > resize_dir_nm "x/y.jpg" == "x/n"
resize_dir_nm :: FilePath -> FilePath
resize_dir_nm f =
    let d = takeDirectory f
    in d </> "n"

mk_resize_dir :: FilePath -> IO ()
mk_resize_dir = createDirectoryIfMissing False . resize_dir_nm

-- > revised_name "x/y.jpg" == "x/n/y.jpg"
revised_name :: FilePath -> FilePath
revised_name f = resize_dir_nm f </> takeFileName f

resize :: Either Int Int -> FilePath -> IO ()
resize dm f = do
  let ofn = revised_name f
      (p,a) = resize_cmd dm f ofn
  e <- doesFileExist ofn
  print (if e then ("-",ofn) else ("+",ofn))
  when (not e) (mk_resize_dir f >> rawSystem p a >> return ())
