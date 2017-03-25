module Graphics.Camera.Rename where

import Data.Char {- base -}
import qualified Data.Time as T {- time -}
import qualified System.Directory as D {- directory -}
import System.FilePath {- filepath -}

import qualified Graphics.Camera.Exif as E {- exif -}

-- > let t = E.exif_parse_time "2008:02:23 12:10:46"
-- > in fmap (mk_name "x/y.jpg") t == Just "x/2008-02-23-12-10-46.jpg"
mk_name :: FilePath -> T.UTCTime -> FilePath
mk_name fn t =
    let d = takeDirectory fn
        x = takeExtension fn
    in  d </> E.exif_format_time t <.> map toLower x

-- | If file has exif @datetime@ data rename file.
rename :: FilePath -> IO ()
rename f = do
  e <- E.exif_read_all_tags f
  case E.exif_time e of
    Just t -> let r = mk_name f t
              in if (f /= r)
                 then print ("+",f,r) >> D.renameFile f r
                 else print ("-",f)
    Nothing -> print ("no-exif",f,e)
