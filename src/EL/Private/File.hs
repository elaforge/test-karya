-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | Do things with files.
-}
module EL.Private.File where
import qualified Control.Exception as Exception
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error


-- | Like 'Directory.getDirectoryContents' except don't return dotfiles and
-- it prepends the directory.
list :: FilePath -> IO [FilePath]
list dir = do
    fns <- Directory.getDirectoryContents dir
    return $ map (strip . (dir </>)) $ filter ((/=".") . take 1) fns
    where
    strip ('.' : '/' : path) = path
    strip path = path

writeLines :: FilePath -> [Text] -> IO ()
writeLines fname lines = IO.withFile fname IO.WriteMode $ \hdl ->
    mapM_ (Text.IO.hPutStrLn hdl) lines

-- * IO errors

-- | If @op@ raised ENOENT, return Nothing.
ignoreEnoent :: IO a -> IO (Maybe a)
ignoreEnoent = ignoreError IO.Error.isDoesNotExistError

ignoreEOF :: IO a -> IO (Maybe a)
ignoreEOF = ignoreError IO.Error.isEOFError

-- | Ignore all IO errors.  This is useful when you want to see if a file
-- exists, because some-file/x will not give ENOENT, but ENOTDIR, which is
-- probably isIllegalOperation.
ignoreIOError :: IO a -> IO (Maybe a)
ignoreIOError = ignoreError (\(_ :: IO.Error.IOError) -> True)

ignoreError :: Exception.Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
ignoreError ignore action = Exception.handleJust (guard . ignore)
    (const (return Nothing)) (fmap Just action)

-- | 'Exception.try' specialized to IOError.
tryIO :: IO a -> IO (Either IO.Error.IOError a)
tryIO = Exception.try
