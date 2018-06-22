{-# LANGUAGE OverloadedStrings #-}
module EL.Private.Cpu where
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Info
import qualified System.Process as Process


physicalCores :: IO Int
physicalCores = case System.Info.os of
    "darwin" -> read <$>
        Process.readProcess "/usr/sbin/sysctl" ["-n", "hw.physicalcpu"] ""
    "linux" -> cpuinfoPhysical <$> Text.IO.readFile "/proc/cpuinfo"
    _ -> error $ "unknown platform: " ++ System.Info.os

-- | Parse /proce/cpuinfo for physical cpu count.
cpuinfoPhysical :: Text -> Int
cpuinfoPhysical = length . unique . map cpu . Text.splitOn "\n\n"
    where
    -- unique pairs of (physical id, core id)
    cpu = filter (\s -> any (`Text.isPrefixOf` s) ["physical id", "core id"])
        . Text.lines

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- if [[ -r /proc/cpuinfo ]]; then
--     # Count unique (physical id, core id) pairs.
--     # Wow linux, seriously?
--     exec egrep 'core id|physical id' /proc/cpuinfo | tr -d '\n' \
--         | sed s/physical/\\nphysical/g | grep -v '^$' | sort | uniq | wc -l
-- elif [[ -x /usr/sbin/sysctl ]]; then # OS X
--     exec /usr/sbin/sysctl -n hw.physicalcpu
-- else
--     echo 1
-- fi

