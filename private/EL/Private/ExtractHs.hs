{-# LANGUAGE OverloadedStrings #-}
-- | A library to collect definitions from haskell source and collect them
-- into an output source file.
module EL.Private.ExtractHs where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import Global


type Error = Text
type Warning = Text

process :: [String] -> (Text -> a)
    -> (FilePath -> Map FilePath a -> ([Warning], Text))
    -> IO ()
process args extract generate = do
    progName <- System.Environment.getProgName
    (outFname, inputs) <- case args of
        outFname : inputs -> return (outFname, inputs)
        _ -> die $ "usage: " <> txt progName
            <> " output.hs input1.hs input2.hs ..."
    extracted <- extractFiles extract inputs
    case generate outFname extracted of
        (warnings, output) -> do
            mapM_ (Text.IO.hPutStrLn IO.stderr) warnings
            Directory.createDirectoryIfMissing True $
                FilePath.takeDirectory outFname
            Text.IO.writeFile outFname $ header progName <> output

header :: String -> Text
header program = "-- automatically generated by " <> txt program <> "\n"

extractFiles :: (Text -> a) -> [FilePath] -> IO (Map FilePath a)
extractFiles extract =
    fmap Map.fromList . mapM (\fn -> (,) fn . extract <$> Text.IO.readFile fn)

-- * extract

typeDeclarations :: Text -> [(Int, (Text, Text))]
typeDeclarations = mapMaybe parse . zip [1..] . Text.lines
    where
    parse (lineno, line)
        | line == "" || Char.isSpace (Text.head line) = Nothing
        | otherwise = case Text.words line of
            name : "::" : rest -> Just (lineno, (name, Text.unwords rest))
            _ -> Nothing

-- | This will be fooled by a {- or -} inside a string.  I don't strip --
-- comments because the extract functions look for left justified text.
stripComments :: Text -> Text
stripComments = mconcat . go (0 :: Int)
    where
    go nesting text
        | Text.null post = [text]
        | "{-" `Text.isPrefixOf` post = (if nesting > 0 then id else (pre:))
            (go (nesting+1) (Text.drop 2 post))
        | otherwise = (if nesting == 0 then (pre <> Text.take 2 post :) else id)
            (go (max 0 (nesting-1)) (Text.drop 2 post))
        where (pre, post) = breakOnFirst "{-" "-}" text

-- | Like 'Text.breakOn', but break on two things.
breakOnFirst :: Text -> Text -> Text -> (Text, Text)
breakOnFirst a b text
    | Text.length aPre <= Text.length bPre = (aPre, aPost)
    | otherwise = (bPre, bPost)
    where
    (aPre, aPost) = Text.breakOn a text
    (bPre, bPost) = Text.breakOn b text

-- * generate

moduleDeclaration :: FilePath -> Text
moduleDeclaration fname = "module " <> pathToModule fname <> " where"

makeImport :: FilePath -> Text
makeImport fname = "import qualified " <> pathToModule fname

pathToModule :: FilePath -> Text
pathToModule =
    Text.map dot . txt . FilePath.dropExtension . FilePath.normalise
    where dot c = if c == '/' then '.' else c

-- * util

die :: Text -> IO a
die msg = do
    Text.IO.hPutStrLn IO.stderr msg
    System.Exit.exitFailure
