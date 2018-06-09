-- | Extra utils for "Data.Map".
module EL.Private.Map where
import qualified Data.Map as Map

import qualified EL.Private.Seq as Seq


-- | Pair up elements from each map with equal keys.
pairs :: Ord k => Map.Map k v1 -> Map.Map k v2 -> [(k, Seq.Paired v1 v2)]
pairs map1 map2 = Seq.pair_sorted (Map.toAscList map1) (Map.toAscList map2)
