-- e0

exp0 m 0 = 0
exp0 m n = m * m `exp0` (n - 1)
-- fails, m ^ 0 != 1

exp1 m 0 = 1
exp1 m n = m * m `exp1` (n - 1)
-- works

exp2 m 0 = 1
exp2 m n = m * m `exp2` n - 1
-- fails, infinite recursion

exp3 m 0 = 1
exp3 m n = n * n `exp3` (m - 1)
-- fails, wrong base and exponent

exp4 m 0 = 1
exp4 m n = m * exp4 m (n - 1)
-- works

exp5 m 0 = 1
exp5 m n = m * m * m `exp5` (n - 2)
-- fails, when n = 1, we get infinite recursion

exp6 m 0 = 1
exp6 m n = (m * m) `exp6` (n -1)
-- fails, wrong order of operations

exp7 m 0 = m
exp7 m n = m * m `exp7` (n - 1)
-- fails, m^0 != 1

-- e1
-- desugaring

-- e2
-- desugaring

-- e3
-- desugaring

-- e4
and0 [] = True
and0 (b:bs) = b && and0 bs
-- works

and1 [] = True
and1 (b:bs)
    | b = and1 bs
    | otherwise = False
-- works

and2 [] = False
and2 (b:bs) = b && and2 bs
-- fails, and [] should be True

and3 [] = False
and3 (b:bs) = b || and3 bs
-- fails, and [] should be True

and4 [] = True
and4 (b:bs)
    | b == False = False
    | otherwise = and4 bs
-- works

and5 [] = True
and5 (b:bs) = b || and5 bs
-- fails, [True,False] returns True

and6 [] = True
and6 (b:bs) = and6 bs && b
-- works

and7 [] = True
and7 (b:bs)
    | b = b
    | otherwise = and7 bs
-- fails, always True

-- e5 --
-- done by inspection

-- e6 --
-- done by inspection

-- e7 --
-- done by inspection

-- e8 --
-- done by inspection

-- e9 --
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- e10 --
halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys,zs) = halve xs
