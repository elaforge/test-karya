{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | Basic testing utilities.
module EL.Test.Testing (
    Config(..), modifyTestConfig, withTestName
    -- * metadata
    , ModuleMeta(..), moduleMeta, Tag(..)
    -- * assertions
    , check, checkVal
    , equal, equalFmt, rightEqual, notEqual, equalf, stringsLike
    , leftLike, match
    , Pattern
    -- ** exception assertions
    , throws

    -- ** io assertions
    , ioEqual

    -- ** low level
    , success, failure

    -- * extracting
    , expectRight

    -- * QuickCheck
    , quickcheck
    , qcEqual

    -- * pretty printing
    , pprint

    -- * filesystem
    , uniqueTmpDir, inTmpDir, tmpBaseDir

    -- * util
    , force
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import Control.Monad (unless)

import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified GHC.Stack as Stack
import GHC.Stack (HasCallStack)
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Temp as Temp
import qualified System.Posix.Terminal as Terminal

import qualified Test.QuickCheck as QuickCheck

import qualified EL.Private.Map as EL.Map
import qualified EL.Private.PPrint as PPrint
import qualified EL.Private.Ranges as Ranges
import qualified EL.Private.Regex as Regex
import qualified EL.Private.Seq as Seq
import qualified EL.Test.ApproxEq as ApproxEq


{-# NOINLINE testConfig #-}
testConfig :: IORef.IORef Config
testConfig = Unsafe.unsafePerformIO $ IORef.newIORef $ Config
    { configTestName = "no-test"
    }

modifyTestConfig :: (Config -> Config) -> IO ()
modifyTestConfig = IORef.modifyIORef testConfig

-- | Set 'configTestName'.  This is a grody hack, but I need it because GHC
-- call stack is off by one, so you get the caller line number, but the
-- callee's function name: https://ghc.haskell.org/trac/ghc/ticket/11686
withTestName :: Text -> IO a -> IO a
withTestName name action = do
    modifyTestConfig (\config -> config { configTestName = name })
    action

data Config = Config {
    -- | Keep the test name so I can report it in 'success' in 'failure'.
    configTestName :: !Text
    } deriving (Show)

check :: HasCallStack => Text -> Bool -> IO Bool
check msg False = failure ("failed: " <> msg)
check msg True = success msg

-- | Check against a function.  Use like:
--
-- > checkVal (f x) $ \case -> ...
checkVal :: Show a => HasCallStack => a -> (a -> Bool) -> IO Bool
checkVal val f
    | f val = success $ "ok: " <> pshowt val
    | otherwise = failure $ "failed: " <> pshowt val

-- * metadata

data ModuleMeta = ModuleMeta {
    -- | Wrap each test with IO level setup and teardown.  Sync exceptions are
    -- caught from the test function, so this should only see async exceptions.
    initialize :: IO () -> IO ()
    , tags :: [Tag]
    }

moduleMeta :: ModuleMeta
moduleMeta = ModuleMeta
    { initialize = id
    , tags = []
    }

data Tag = Large -- ^ Especially expensive to run.
    deriving (Eq, Show)

-- * equal and diff

equal :: (HasCallStack, Show a, Eq a) => a -> a -> IO Bool
equal a b
    | a == b = success $ cmp True
    | otherwise = failure $ cmp False
    where cmp = prettyCompare "==" "/=" True a b

equalFmt :: (HasCallStack, Eq a, Show a) => (a -> Text) -> a -> a -> IO Bool
equalFmt fmt a b = do
    ok <- equal a b
    let (pa, pb) = (fmt a, fmt b)
    unless (ok || Text.null pa && Text.null pb) $
        Text.IO.putStrLn $ showDiff pa pb
    return ok
    where
    showDiff prettyA prettyB = fmtLines "/="
        (Text.lines $ highlightLines color diffA prettyA)
        (Text.lines $ highlightLines color diffB prettyB)
        where
        color = failureColor
        (diffA, diffB) = diffRanges prettyA prettyB

notEqual :: (HasCallStack, Show a, Eq a) => a -> a -> IO Bool
notEqual a b
    | a == b = failure $ cmp True
    | otherwise = success $ cmp False
    where cmp = prettyCompare "==" "/=" False a b

rightEqual :: (HasCallStack, Show err, Show a, Eq a) => Either err a -> a
    -> IO Bool
rightEqual (Right a) b = equal a b
rightEqual (Left err) _ = failure $ "Left: " <> pshowt err

-- | Show the values nicely, whether they are equal or not.
prettyCompare :: Show a =>
    Text -- ^ equal operator
    -> Text -- ^ inequal operator
    -> Bool -- ^ If True then equal is expected so inequal will be highlighted
    -- red.  Otherwise, inequal is expected and highlighted green.
    -> a -> a -> Bool -- ^ True if as are equal
    -> Text
prettyCompare equal inequal expectEqual a b isEqual
    | isEqual = equal <> " " <> ellipse (showt a)
    | otherwise = fmtLines inequal
        (Text.lines $ highlightLines color diffA prettyA)
        (Text.lines $ highlightLines color diffB prettyB)
    where
    color = if expectEqual then failureColor else successColor
    (diffA, diffB) = diffRanges prettyA prettyB
    prettyA = Text.strip $ pshowt a
    prettyB = Text.strip $ pshowt b
    -- Equal values are usually not interesting, so abbreviate if they're too
    -- long.
    ellipse s
        | len > maxlen = Text.take maxlen s <> "... {" <> showt len <> "}"
        | otherwise = s
        where len = Text.length s
    maxlen = 200

-- | Apply color ranges as produced by 'diffRanges'.
highlightLines :: ColorCode -> IntMap.IntMap [CharRange] -> Text -> Text
highlightLines color nums = Text.unlines . zipWith hi [0..] . Text.lines
    where
    hi i line = case IntMap.lookup i nums of
        Just ranges -> highlightRanges color ranges line
        Nothing -> line

highlightRanges :: ColorCode -> [CharRange] -> Text -> Text
highlightRanges color ranges = mconcat . map hi . splitRanges ranges
    where hi (outside, inside) = outside <> highlight color inside

splitRanges :: [(Int, Int)] -> Text -> [(Text, Text)] -- ^ (out, in) pairs
splitRanges ranges = go 0 ranges
    where
    go _ [] text
        | Text.null text = []
        | otherwise = [(text, mempty)]
    go prev ((s, e) : ranges) text = (pre, within) : go e ranges post
        where
        (pre, rest) = Text.splitAt (s-prev) text
        (within, post) = Text.splitAt (e - s) rest

type CharRange = (Int, Int)

diffRanges :: Text -> Text
    -> (IntMap.IntMap [CharRange], IntMap.IntMap [CharRange])
diffRanges first second =
    toMap $ Seq.partition_paired $ map diffLine $
        EL.Map.pairs firstByLine secondByLine
    where
    toMap (as, bs) = (IntMap.fromList as, IntMap.fromList bs)
    diffLine (num, d) = case d of
        Seq.Both line1 line2 -> Seq.Both (num, d1) (num, d2)
            where (d1, d2) = charDiff line1 line2
        Seq.First line1 -> Seq.First (num, [(0, Text.length line1)])
        Seq.Second line2 -> Seq.Second (num, [(0, Text.length line2)])
    firstByLine = Map.fromList
        [(n, text) | Diff.First (Numbered n text) <- diffs]
    secondByLine = Map.fromList
        [(n, text) | Diff.Second (Numbered n text) <- diffs]
    diffs = numberedDiff (==) (Text.lines first) (Text.lines second)

charDiff :: Text -> Text -> ([CharRange], [CharRange])
charDiff first second
    | tooDifferent firstCs || tooDifferent secondCs =
        ([(0, Text.length first)], [(0, Text.length second)])
    | otherwise = (firstCs, secondCs)
    where
    firstCs = toRanges [n | Diff.First (Numbered n _) <- diffs]
    secondCs = toRanges [n | Diff.Second (Numbered n _) <- diffs]
    diffs = numberedDiff (==) (Text.unpack first) (Text.unpack second)
    -- If there are too many diff ranges let's just mark the whole thing
    -- different.  Perhaps I should ignore spaces that are the same, but let's
    -- see how this work first.
    tooDifferent ranges = length ranges > 2

toRanges :: [Int] -> [(Int, Int)]
toRanges xs = Ranges.merge_sorted [(n, n+1) | n <- xs]

numberedDiff :: (a -> a -> Bool) -> [a] -> [a] -> [Diff.Diff (Numbered a)]
numberedDiff equal a b =
    Diff.getDiffBy (\a b -> _numberedVal a `equal` _numberedVal b)
        (number a) (number b)
    where number = zipWith Numbered [0..]

data Numbered a = Numbered {
    _numbered :: !Int
    , _numberedVal :: !a
    } deriving (Show)

-- * approximately equal

equalf :: (HasCallStack, Show a, ApproxEq.ApproxEq a) => Double -> a -> a
    -> IO Bool
equalf eta a b
    | ApproxEq.eq eta a b = success $ pretty True
    | otherwise = failure $ pretty False
    where pretty = prettyCompare "~~" "/~" True a b

-- * other assertions

class Show a => TextLike a where toText :: a -> Text
instance TextLike String where toText = Text.pack
instance TextLike Text where toText = id

-- | Strings in the first list match patterns in the second list, using
-- 'patternMatches'.
stringsLike :: forall txt. (HasCallStack, TextLike txt) => [txt] -> [Pattern]
    -> IO Bool
stringsLike gotten_ expected
    | all isBoth diffs = success $ fmtLines "=~" gotten expected
    | otherwise = failure $ fmtLines "/~"
        (map (fmtLine (Set.fromList [_numbered a | Diff.Second a <- diffs]))
            (zip [0..] gotten))
        (map (fmtLine (Set.fromList [_numbered a | Diff.First a <- diffs]))
            (zip [0..] expected))
    where
    fmtLine failures (n, line)
        | Set.member n failures = highlight failureColor line
        | otherwise = line
    gotten = map toText gotten_
    diffs = numberedDiff patternMatches expected gotten
    isBoth (Diff.Both {}) = True
    isBoth _ = False

-- | Format multiple lines with an operator between them, on a single line if
-- they fit.
fmtLines :: Text -> [Text] -> [Text] -> Text
fmtLines operator [x] [y] | Text.length x + Text.length y <= 70 =
    x <> " " <> operator <> " " <> y
fmtLines operator [] [y] = "<empty> " <> operator <> " " <> y
fmtLines operator [x] [] = x <> " " <> operator <> " <empty>"
fmtLines operator xs ys = ("\n"<>) $ Text.stripEnd $
    Text.unlines $ xs <> ["    " <> operator] <> ys

-- | It's common for Left to be an error msg, or be something that can be
-- converted to one.
leftLike :: (HasCallStack, Show a, TextLike txt) => Either txt a -> Pattern
    -> IO Bool
leftLike gotten expected = case gotten of
    Left msg
        | patternMatches expected msg -> success $
            "Left " <> toText msg <> " =~ Left " <> toText expected
        | otherwise ->
            failure $ "Left " <> toText msg <> " !~ Left " <> toText expected
    Right a ->
        failure $ "Right (" <> showt a <> ") !~ Left " <> toText expected

match :: (HasCallStack, TextLike txt) => txt -> Pattern -> IO Bool
match gotten pattern =
    (if matches then success else failure) $
        fmtLines (if matches then "=~" else "!~")
            (Text.lines (toText gotten)) (Text.lines pattern)
    where
    matches = patternMatches pattern gotten

-- | Pattern as matched by 'patternMatches'.
type Pattern = Text

-- | This is a simplified pattern that only has the @*@ operator, which is
-- equivalent to regex's @.*?@.  This reduces the amount of quoting you have
-- to write.  You can escape @*@ with a backslash.
patternMatches :: TextLike txt => Pattern -> txt -> Bool
patternMatches pattern = not . null . Regex.groups (patternToRegex pattern)
    . toText

patternToRegex :: HasCallStack => Text -> Regex.Regex
patternToRegex =
    Regex.compileOptionsUnsafe [Regex.DotAll] . mkstar . Regex.escape
        . Text.unpack
    where
    mkstar "" = ""
    mkstar ('\\' : '\\' : '\\' : '*' : cs) = '\\' : '*' : mkstar cs
    mkstar ('\\' : '*' : cs) = '.' : '*' : '?' : mkstar cs
    mkstar (c : cs) = c : mkstar cs

-- | The given pure value should throw an exception that matches the predicate.
throws :: (HasCallStack, Show a) => a -> Pattern -> IO Bool
throws val excPattern =
    (Exception.evaluate val >> failure ("didn't throw: " <> showt val))
    `Exception.catch` \(exc :: Exception.SomeException) ->
        if patternMatches excPattern (showt exc)
            then success ("caught exc: " <> showt exc)
            else failure $ "exception <" <> showt exc <> "> didn't match "
                <> excPattern

ioEqual :: (HasCallStack, Eq a, Show a) => IO a -> a -> IO Bool
ioEqual ioVal expected = do
    val <- ioVal
    equal val expected

expectRight :: (HasCallStack, Show a) => Either a b -> b
expectRight (Left v) = error (pshow v)
expectRight (Right v) = v

-- * QuickCheck

-- | Run a quickcheck property.
quickcheck :: (HasCallStack, QuickCheck.Testable prop) => prop -> IO Bool
quickcheck prop = do
    (ok, msg) <- fmtQuickCheckResult <$>
        QuickCheck.quickCheckWithResult args prop
    (if ok then success else failure) msg
    where
    args = QuickCheck.stdArgs { QuickCheck.chatty = False }

fmtQuickCheckResult :: QuickCheck.Result -> (Bool, Text)
fmtQuickCheckResult result = fmap Text.strip $ case result of
    QuickCheck.Success { output } -> (True, Text.pack output)
    QuickCheck.GaveUp { output } -> (False, Text.pack output)
    QuickCheck.Failure { output } -> (False, Text.pack output)
    QuickCheck.NoExpectedFailure { output } -> (False, Text.pack output)
#if ! MIN_VERSION_QuickCheck(2, 12, 0)
    QuickCheck.InsufficientCoverage { output } -> (False, Text.pack output)
#endif

-- | 'equal' for quickcheck.
qcEqual :: (Show a, Eq a) => a -> a -> QuickCheck.Property
qcEqual a b = QuickCheck.counterexample
    (Text.unpack $ prettyCompare "==" "/=" True a b False)
    (a == b)

-- * util

-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

-- | Print a msg with a special tag indicating a passing test.
success :: HasCallStack => Text -> IO Bool
success msg = do
    printTestLine Stack.callStack successColor "++-> " msg
    return True

-- | Print a msg with a special tag indicating a failing test.
failure :: HasCallStack => Text -> IO Bool
failure msg = do
    printTestLine Stack.callStack failureColor "__-> " msg
    return False

printTestLine :: Stack.CallStack -> ColorCode -> Text -> Text -> IO ()
printTestLine stack color prefix msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    isatty <- Terminal.queryTerminal IO.stdOutput
    testName <- configTestName <$> IORef.readIORef testConfig
    -- If there is highlighting in the msg, then it's probably a diff so
    -- most of it may be unhighlighted.  So highlight the prefix to make
    -- the line visible.
    let fullPrefix = (if isHighlighted msg then highlight color else id)
            (prefix <> showStack testName stack)
    let fullMsg = fullPrefix <> " " <> msg
        highlighted
            -- I only want colors in tty output.
            | not isatty = stripColors fullMsg
            -- Don't put on a color if it already has some.
            | isHighlighted fullMsg = fullMsg
            | otherwise = highlight color fullMsg
    Text.IO.putStrLn highlighted
    where
    isHighlighted = (vt100Prefix `Text.isInfixOf`)

showStack :: Text -> Stack.CallStack -> Text
showStack testName =
    maybe "<empty-stack>" showFrame . Seq.last . Stack.getCallStack
    where
    showFrame (_, srcloc) =
        Text.pack (Stack.srcLocFile srcloc) <> ":"
        <> showt (Stack.srcLocStartLine srcloc)
        <> if Text.null testName then "" else " [" <> testName <> "]"

highlight :: ColorCode -> Text -> Text
highlight (ColorCode code) text
    | Text.null text = text
    | otherwise = code <> text <> vt100Normal

-- | Remove vt100 color codes.
stripColors :: Text -> Text
stripColors = mconcat . Seq.map_tail (Text.drop 1 . Text.dropWhile (/='m'))
    . Text.splitOn vt100Prefix

newtype ColorCode = ColorCode Text deriving (Show)

vt100Prefix :: Text
vt100Prefix = "\ESC["

vt100Normal :: Text
vt100Normal = "\ESC[m\ESC[m"

-- | These codes should probably come from termcap, but I can't be bothered.
failureColor :: ColorCode
failureColor = ColorCode "\ESC[31m" -- red

successColor :: ColorCode
successColor = ColorCode "\ESC[32m" -- green

-- * pretty

pprint :: Show a => a -> IO ()
pprint = putStr . pshow

showt :: Show a => a -> Text
showt = Text.pack . show

pshowt :: Show a => a -> Text
pshowt = Text.pack . pshow

-- | Strict pshow, so I don't get debug traces interleaved with printing.
pshow :: Show a => a -> String
pshow val = s `DeepSeq.deepseq` s
    where s = PPrint.pshow val

-- * filesystem

-- | Get a tmp dir, which will be unique for each test run.
uniqueTmpDir :: String -> IO FilePath
uniqueTmpDir prefix = do
    Directory.createDirectoryIfMissing True tmpBaseDir
    Temp.mkdtemp $ tmpBaseDir </> prefix ++ "-"

-- | Run the computation with cwd in a new tmp dir.
inTmpDir :: String -> IO a -> IO a
inTmpDir prefix action = do
    dir <- uniqueTmpDir prefix
    Directory.withCurrentDirectory dir action

-- | All tmp files used by tests should go in this directory.
--
-- TODO instead of being hardcoded this should be configured per-project.
tmpBaseDir :: FilePath
tmpBaseDir = "dist/test-tmp"

-- * util

force :: DeepSeq.NFData a => a -> IO ()
force x = Exception.evaluate (DeepSeq.rnf x)
