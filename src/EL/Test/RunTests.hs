{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Run tests.  This is meant to be invoked via a main module generated by
-- "EL.Test.GenerateRunTests".
module EL.Test.RunTests where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Fix as Fix

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.IO as Text.Lazy.IO

import qualified Numeric
import qualified System.CPUTime as CPUTime
import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Environment
import qualified System.Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Text.Read as Read

import qualified EL.Private.Cpu as Cpu
import qualified EL.Private.File as File
import qualified EL.Private.Process as EL.Process
import qualified EL.Private.Regex as Regex
import qualified EL.Private.Seq as Seq
import qualified EL.Test.Testing as Testing

import Global


data Test = Test {
    -- | Name of the test function.
    testSymName :: Text
    -- | Run the test.
    , testRun :: IO ()
    -- | Test module filename.
    , testFilename :: FilePath
    -- | Line of the test function declaration.
    , testLine :: Int
    -- | Module-level metadata, declared as @meta@ in the test module toplevel.
    , testModuleMeta_ :: Maybe Testing.ModuleMeta
    }

testModuleMeta :: Test -> Testing.ModuleMeta
testModuleMeta = Maybe.fromMaybe Testing.moduleMeta . testModuleMeta_

testName :: Test -> Text
testName test = Text.intercalate "," tags <> "-" <> testSymName test
    where
    tags = if null tags_ then ["normal"] else tags_
    tags_ = Seq.unique_sort $ map (Text.toLower . showt) $
        Testing.tags (testModuleMeta test)

-- Prefix for lines with test metadata.
metaPrefix :: Text
metaPrefix = "===>"

data Flag =
    CheckOutput
    | ClearDirs
    | Jobs !Jobs
    | List
    | Output !FilePath
    | Subprocess
    deriving (Eq, Show)

data Jobs = Auto | NJobs !Int deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["check-output"] (GetOpt.NoArg CheckOutput)
        "Check output for failures after running. Only valid with --output."
    , GetOpt.Option [] ["clear-dirs"] (GetOpt.NoArg ClearDirs)
        "Remove everything in the test tmp dir and --output.\
        \ This is probably just for cabal, which can't wrap tests in a shell\
        \ script."
    , GetOpt.Option [] ["jobs"] (GetOpt.ReqArg (Jobs . parseJobs) "1")
        "Number of parallel jobs, or 'auto' for physical CPU count."
    , GetOpt.Option [] ["list"] (GetOpt.NoArg List) "display but don't run"
    , GetOpt.Option [] ["output"] (GetOpt.ReqArg Output "path")
        "Path to a directory to put output logs, if not given output goes to\
        \ stdout."
    , GetOpt.Option [] ["subprocess"] (GetOpt.NoArg Subprocess)
        "Read test names on stdin.  This is meant to be run as a subprocess\
        \ by --jobs."
    ]
    where
    parseJobs s
        | s == "auto" = Auto
        | Just n <- Read.readMaybe s = NJobs n
        | otherwise = error $ "jobs should be auto or a number, was: " <> show s

-- | Called by the generated main function.
run :: [String] -> [Test] -> IO ()
run defaultArgs allTests = do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    args <- System.Environment.getArgs
    args <- return $ if null args then defaultArgs else args
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (opts, n, []) -> return (opts, n)
        (_, _, errors) -> quitWithUsage defaultArgs errors
    ok <- runTests allTests flags args
    if ok then System.Exit.exitSuccess else System.Exit.exitFailure

quitWithUsage :: [String] -> [String] -> IO a
quitWithUsage defaultArgs errors = do
    progName <- System.Environment.getProgName
    putStrLn $ "usage: " <> progName <> " [ flags ] regex regex ..."
    putStr $ GetOpt.usageInfo "Run tests that match any regex." options
    unless (null defaultArgs) $
        putStrLn $ "\ndefault args provided by generator:\n"
            <> unwords defaultArgs
    unless (null errors) $
        putStr $ "\nerrors:\n" <> unlines errors
    System.Exit.exitFailure

runTests :: [Test] -> [Flag] -> [String] -> IO Bool
runTests allTests flags regexes = do
    when (mbOutputDir == Nothing && CheckOutput `elem` flags) $
        quitWithUsage [] ["--check-output requires --output"]
    when (ClearDirs `elem` flags) $ do
        clearDirectory Testing.tmpBaseDir
        whenJust mbOutputDir clearDirectory
    Directory.createDirectoryIfMissing True Testing.tmpBaseDir
    if  | List `elem` flags -> do
            mapM_ Text.IO.putStrLn $ List.sort $ map testName $
                if null regexes then allTests else matches
            return True
        | Subprocess `elem` flags -> subprocess allTests >> return True
        | Just outputDir <- mbOutputDir -> do
            jobs <- getJobs $
                fromMaybe (NJobs 1) $ Seq.last [n | Jobs n <- flags]
            runOutput outputDir jobs matches (CheckOutput `elem` flags)
        | otherwise -> mapM_ runTest matches >> return True
    where
    mbOutputDir = Seq.last [d | Output d <- flags]
    matches = matchingTests regexes allTests

getJobs :: Jobs -> IO Int
getJobs (NJobs n) = return n
getJobs Auto = Cpu.physicalCores

runOutput :: FilePath -> Int -> [Test] -> Bool -> IO Bool
runOutput outputDir jobs tests check = do
    Directory.createDirectoryIfMissing True outputDir
    let outputs = [outputDir </> "out" <> show n <> ".stdout" | n <- [1..jobs]]
    runParallel outputs tests
    if check
        then checkOutputs outputs
        else return True
    -- TODO run hpc?

-- | Isolate the test by running it in a subprocess.  I'm not sure if this is
-- necessary, but I believe at the time GUI-using tests would crash each other
-- without it.  Presumably they left some GUI state around that process exit
-- will clean up.
runInSubprocess :: Test -> IO ()
runInSubprocess test = do
    argv0 <- System.Environment.getExecutablePath
    putStrLn $ "subprocess: " ++ show argv0 ++ " " ++ show [testName test]
    val <- Process.rawSystem argv0 [untxt (testName test)]
    case val of
        System.Exit.ExitFailure code -> Testing.withTestName (testName test) $
            void $ Testing.failure $
                "test returned " <> showt code <> ": " <> testName test
        _ -> return ()

-- * parallel jobs

-- | Run tests in parallel, redirecting stdout and stderr to each output.
runParallel :: [FilePath] -> [Test] -> IO ()
runParallel _ [] = return ()
runParallel outputs tests = do
    let byModule = Seq.keyed_group_adjacent testFilename tests
    queue <- newQueue [(txt name, tests) | (name, tests) <- byModule]
    Async.forConcurrently_ (map fst (zip outputs byModule)) $
        \output -> jobThread output queue

-- | Pull tests off the queue and feed them to a single subprocess.
jobThread :: FilePath -> Queue (Text, [Test]) -> IO ()
jobThread output queue =
    Exception.bracket (IO.openFile output IO.AppendMode) IO.hClose $ \hdl -> do
        to <- Chan.newChan
        env <- Environment.getEnvironment
        argv0 <- System.Environment.getExecutablePath
        -- Give each subprocess its own .tix, or they will stomp on each other
        -- and crash.
        from <- EL.Process.conversation argv0 ["--subprocess"]
            (Just (("HPCTIXFILE", output <> ".tix") : env)) to
        whileJust (takeQueue queue) $ \(name, tests) -> do
            put $ untxt name
            Chan.writeChan to $ EL.Process.Text $
                Text.unwords (map testName tests) <> "\n"
            Fix.fix $ \loop -> Chan.readChan from >>= \case
                EL.Process.Stdout line
                    | line == testsCompleteLine -> return ()
                    | otherwise -> Text.IO.hPutStrLn hdl line >> loop
                EL.Process.Stderr line -> Text.IO.hPutStrLn hdl line >> loop
                EL.Process.Exit n -> put $ "completed early: " <> show n
        Chan.writeChan to EL.Process.EOF
        final <- Chan.readChan from
        case final of
            EL.Process.Exit n
                | n == 0 -> return ()
                | otherwise -> put $ "completed " <> show n
            _ -> put $ "expected Exit, but got " <> show final
    where
    put = putStr . ((output <> ": ")<>) . (<>"\n")

subprocess :: [Test] -> IO ()
subprocess allTests = void $ File.ignoreEOF $ forever $ do
    testNames <- Set.fromList . Text.words <$> Text.IO.getLine
    -- For some reason, I get an extra "" from getLine when the parent process
    -- closes the pipe.  From the documentation I think it should throw EOF.
    unless (Set.null testNames) $ do
        let tests = filter ((`Set.member` testNames) . testName) allTests
        mapM_ runTest tests
            `Exception.finally` Text.IO.hPutStrLn IO.stdout testsCompleteLine
            -- I previously wrote this on stderr, but it turns out it then
            -- gets nondeterministically interleaved with stdout.

-- | Signal to the caller that the current batch of tests are done.
testsCompleteLine :: Text
testsCompleteLine = "•complete•"

-- * run tests

-- | Match all tests whose names match any regex, or if a test is an exact
-- match, just that test.
matchingTests :: [String] -> [Test] -> [Test]
matchingTests regexes tests = concatMap match regexes
    where
    match reg = case List.find ((== txt reg) . testName) tests of
        Just test -> [test]
        Nothing -> filter (Regex.matches (Regex.compileUnsafe reg) . testName)
            tests

runTest :: Test -> IO ()
runTest test = Testing.withTestName name $ isolate $ do
    Text.IO.putStrLn $ Text.unwords [metaPrefix, "run-test", testName test]
    start <- CPUTime.getCPUTime
    Testing.initialize (testModuleMeta test) $
        catch (testSymName test) (testRun test)
    end <- CPUTime.getCPUTime
    -- CPUTime is in picoseconds.
    let secs = fromIntegral (end - start) / 10^12
    -- Grep for timing to make a histogram.
    Text.IO.putStrLn $ Text.unwords [metaPrefix, "timing", testName test,
        txt $ Numeric.showFFloat (Just 3) secs ""]
    return ()
    where name = last (Text.split (=='.') (testName test))

-- | Try to save and restore any process level state in case the test messes
-- with it.  Currently this just restores CWD, but probably there is more than
-- that.  For actual isolation probably a subprocess is necessary.
isolate :: IO a -> IO a
isolate = Directory.withCurrentDirectory "."

catch :: Text -> IO a -> IO ()
catch name action = do
    result <- Exception.try action
    case result of
        Left (exc :: Exception.SomeException) -> do
            void $ Testing.failure $ name <> " threw exception: " <> showt exc
            -- Die on async exception, otherwise it will try to continue
            -- after ^C or out of memory.
            case Exception.fromException exc of
                Just (exc :: Exception.AsyncException) -> Exception.throwIO exc
                Nothing -> return ()
        Right _ -> return ()

-- * queue

-- | This is a simple channel which is written to once, and read from until
-- empty.
newtype Queue a = Queue (MVar.MVar [a])

newQueue :: [a] -> IO (Queue a)
newQueue = fmap Queue . MVar.newMVar

takeQueue :: Queue a -> IO (Maybe a)
takeQueue (Queue mvar) = MVar.modifyMVar mvar $ \as -> return $ case as of
    [] -> ([], Nothing)
    a : as -> (as, Just a)

whileJust :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJust get action = Fix.fix $ \loop -> get >>= \case
    Nothing -> return ()
    Just a -> action a >> loop

-- * check output

-- | Empty the directory, but don't remove it entirely, in case it's /tmp or
-- something.
clearDirectory :: FilePath -> IO ()
clearDirectory dir = void . File.ignoreEnoent $ mapM_ rm =<< File.list dir
    where
    -- Let's not go all the way to Directory.removePathForcibly.
    rm fn = Directory.doesDirectoryExist fn >>= \isDir -> if isDir
        then Directory.removeDirectoryRecursive fn
        else Directory.removeFile fn

checkOutputs :: [FilePath] -> IO Bool
checkOutputs outputs = do
    (failureContext, failures, checks, tests) <-
        extractStats <$> concatMapM readFileEmpty outputs
    unless (null failureContext) $ putStrLn "\n*** FAILURES:"
    mapM_ Text.IO.putStrLn failureContext
    Text.IO.putStrLn $ showt failures <> " failed / "
        <> showt checks <> " checks / " <> showt tests <> " tests"
    return $ failures == 0

readFileEmpty :: FilePath -> IO Text.Lazy.Text
readFileEmpty = fmap (fromMaybe "") . File.ignoreEnoent . Text.Lazy.IO.readFile

extractStats :: Text.Lazy.Text -> ([Text], Int, Int, Int)
extractStats = collect . drop 1 . Seq.split_before isTest . Text.Lazy.lines
    where
    collect tests = (failures, length failures, length extracted, length tests)
        where
        failures = Maybe.catMaybes extracted
        extracted = concatMap extractFailures tests
    isTest = ((Text.Lazy.fromStrict (metaPrefix <> " run-test"))
        `Text.Lazy.isPrefixOf`)

-- | Collect lines before each failure for context.
extractFailures :: [Text.Lazy.Text] -> [Maybe Text]
extractFailures =
    mapMaybe fmt . Seq.split_after (\x -> isFailure x || isSuccess x)
    where
    fmt lines
        | maybe False isFailure (Seq.last lines) =
            Just $ Just $ Text.Lazy.toStrict (Text.Lazy.unlines lines)
        | maybe False isSuccess (Seq.last lines) = Just Nothing
        | otherwise = Nothing
    isFailure = ("__-> " `Text.Lazy.isPrefixOf`)
    isSuccess = ("++-> " `Text.Lazy.isPrefixOf`)
