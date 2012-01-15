module Graphics.Camera.Rename where

import qualified Graphics.Camera.Exif as E
import System.Directory {- directory -}
import System.FilePath {- filepath -}

-- > mk_name "x/y.jpg" "2008:02:23 12:10:46" == "x/2008-02-23-12-10-46.jpg"
mk_name :: FilePath -> String -> FilePath
mk_name f t =
    let d = takeDirectory f
        x = takeExtension f
    in d </> E.fmt_time (E.prs_time t) <.> x

-- | If file has exif @datetime@ data rename file.
rename :: FilePath -> IO ()
rename f = do
  let act = renameFile -- P.createSymbolicLink {-renameFile-}
  e <- E.read_all_tags f
  case E.exif_datetime e of
    Just (_,d) -> print f >> act f (mk_name f d)
    Nothing -> print e
