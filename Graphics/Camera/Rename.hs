module Graphics.Camera.Rename where

import Data.Char {- base -}
import Data.Time {- time -}
import qualified Graphics.Camera.Exif as E {- exif -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

-- > let t = E.prs_time "2008:02:23 12:10:46"
-- > in fmap (mk_name "x/y.jpg") t == Just "x/2008-02-23-12-10-46.jpg"
mk_name :: FilePath -> UTCTime -> FilePath
mk_name fn t =
    let d = takeDirectory fn
        x = takeExtension fn
    in  d </> E.fmt_time t <.> map toLower x

-- | If file has exif @datetime@ data rename file.
rename :: FilePath -> IO ()
rename f = do
  e <- E.read_all_tags f
  case E.exif_time e of
    Just t -> let r = mk_name f t
              in if (f /= r)
                 then print ("+",f,r) >> renameFile f r
                 else print ("-",f)
    Nothing -> print ("no-exif",f,e)
