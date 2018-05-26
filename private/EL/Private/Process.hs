-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Utilities to deal with processes.
module EL.Private.Process where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad (forever, void)

import qualified Data.String as String
import qualified Data.Text.IO as Text.IO
import qualified System.Exit
import qualified System.IO as IO
import qualified System.Process as Process

import qualified EL.Private.File as File
import Global


data TalkOut = Stdout !Text | Stderr !Text | Exit !Int
    deriving (Eq, Show)
data TalkIn = Text !Text | EOF
    deriving (Eq, Show)

instance String.IsString TalkIn where
    fromString = Text . txt

-- | Have a conversation with a subprocess.  This doesn't use ptys, so this
-- will only work if the subprocess explicitly doesn't use block buffering.
conversation :: FilePath -> [String] -> Maybe [(String, String)]
    -> Chan.Chan TalkIn -> IO (Chan.Chan TalkOut)
conversation cmd args env input = do
    output <- Chan.newChan
    Concurrent.forkIO $ Process.withCreateProcess proc $
        \(Just stdin) (Just stdout) (Just stderr) pid -> do
            Concurrent.forkIO $ void $ File.ignoreEOF $ forever $
                Chan.writeChan output . Stdout =<< Text.IO.hGetLine stdout
            Concurrent.forkIO $ void $ File.ignoreEOF $ forever $
                Chan.writeChan output . Stderr =<< Text.IO.hGetLine stderr
            Concurrent.forkIO $ forever $ Chan.readChan input >>= \case
                Text t -> Text.IO.hPutStrLn stdin t >> IO.hFlush stdin
                EOF -> IO.hClose stdin
            code <- Process.waitForProcess pid
            case code of
                System.Exit.ExitFailure code ->
                    Chan.writeChan output (Exit code)
                _ -> Chan.writeChan output (Exit 0)
    return output
    where
    proc = (Process.proc cmd args)
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        , Process.close_fds = True
        , Process.env = env
        }
