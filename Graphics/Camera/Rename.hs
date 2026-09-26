-- | Rename
module Graphics.Camera.Rename where

import qualified Control.Monad {- base -}
import qualified Data.Char {- base -}

import qualified Data.Time {- time -}
import qualified System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Graphics.Camera.Exif as Exif {- exif -}

{- | Mk name

>>> let t = Exif.exif_parse_time Data.Time.utc "2008:02:23 12:10:46"
>>> fmap (mk_name "./x.jpg") t
Just "2008-02-23-12-10-46.jpg"

>>> fmap (mk_name "x/y.jpg") t
Just "x/2008-02-23-12-10-46.jpg"
-}
mk_name :: FilePath -> Data.Time.UTCTime -> FilePath
mk_name fn t =
  let d = takeDirectory fn
      x = takeExtension fn
  in normalise (d </> Exif.exif_format_time t <.> map Data.Char.toLower x)

rename_file_if_exists :: FilePath -> FilePath -> IO ()
rename_file_if_exists p q = do
  x <- System.Directory.doesFileExist p
  Control.Monad.when x (System.Directory.renameFile p q)

{- | If file has exif @datetime@ data rename file.  If there is an
associated @meta@ file rename it as well.
-}
rename_tz :: Data.Time.TimeZone -> FilePath -> IO ()
rename_tz z fn = do
  e <- Exif.exif_read_all_tags fn
  let fn_meta = dropExtension fn <.> "meta"
  case Exif.exif_time z e of
    Just tm ->
      let fn_tm = mk_name fn tm
      in if (fn /= fn_tm)
          then
            print ("+", fn, fn_tm)
              >> System.Directory.renameFile fn fn_tm
              >> rename_file_if_exists fn_meta (mk_name fn_meta tm)
          else print ("-", fn)
    Nothing -> print ("no-exif", fn, e)

rename :: FilePath -> IO ()
rename = rename_tz Data.Time.utc
