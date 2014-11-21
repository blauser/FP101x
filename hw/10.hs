import Test.QuickCheck

-- e0 --
-- done in countdown.lhs

-- e1 --
removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y : ys)
    | x == y = ys
    | otherwise = y : removeone x ys

-- e2 --
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) [] = False
isChoice (x : xs) ys = elem x ys && isChoice xs (removeone x ys)

-- e4 --
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

prop_Split xs =
    and [ls ++ rs == xs | (ls, rs) <- split xs]
