module Graphics.Camera.Resize where

import Control.Monad {- base -}
import qualified System.Directory as D {- directory -}
import System.FilePath {- filepath -}
import qualified System.Process as P {- process -}

type Command = (String, [String])

type Dimension = Either Int Int

resize_arg :: Dimension -> String
resize_arg dm =
  case dm of
    Left w -> show w ++ "x"
    Right h -> "x" ++ show h

{- | Resize cmd

>>> resize_cmd (Right 200) "x.jpg" "r/200/x.jpg"
("convert",["-resize","x200","-colorspace","rgb","x.jpg","r/200/x.jpg"])
-}
resize_cmd :: Dimension -> FilePath -> FilePath -> Command
resize_cmd dm i_fn o_fn =
  ( "convert"
  ,
    [ "-resize"
    , resize_arg dm
    , "-colorspace"
    , "rgb"
    , i_fn
    , o_fn
    ]
  )

{- | Resize dir nm

>>> resize_dir_nm 150 "./x/y.jpg"
"x/r/150"
-}
resize_dir_nm :: Int -> FilePath -> FilePath
resize_dir_nm n fn =
  let d = takeDirectory fn
  in normalise (d </> "r" </> show n)

mk_resize_dir :: Int -> FilePath -> IO ()
mk_resize_dir n = D.createDirectoryIfMissing False . resize_dir_nm n

{- | Revised name

>>> revised_name 150 "x/y.jpg"
"x/r/150/y.jpg"
-}
revised_name :: Int -> FilePath -> FilePath
revised_name n fn = resize_dir_nm n fn </> takeFileName fn

resize :: Either Int Int -> FilePath -> IO ()
resize dm f = do
  let sz = either id id dm
      o_fn = revised_name sz f
      (p, a) = resize_cmd dm f o_fn
  e <- D.doesFileExist o_fn
  print (if e then ("-", o_fn) else ("+", o_fn))
  when (not e) (mk_resize_dir sz f >> P.rawSystem p a >> return ())
