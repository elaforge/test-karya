module EL.Private.Ranges where


merge_sorted :: Ord n => [(n, n)] -> [(n, n)]
merge_sorted [] = []
merge_sorted [x] = [x]
merge_sorted ((s1, e1) : (s2, e2) : rest)
    | e1 >= e2 = merge_sorted ((s1, e1) : rest)
    | e1 >= s2 = merge_sorted ((s1, e2) : rest)
    | otherwise = (s1, e1) : merge_sorted ((s2, e2) : rest)
