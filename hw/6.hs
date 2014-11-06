-- e0 --
-- done by inspection

-- for e1-4
testBoolFunc :: Eq c => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
testBoolFunc f g p xs = f p xs == g p xs

-- e1 -- 
all0 :: (a -> Bool) -> [a] -> Bool
all0 p xs = and (map p xs)
-- works

-- all1 :: (a -> Bool) -> [a] -> Bool
-- all1 p xs = map p (and xs)
-- type error, and return Bool, map needs [Bool]

all2 :: (a -> Bool) -> [a] -> Bool
all2 p = and . map p
-- works

all3 :: (a -> Bool) -> [a] -> Bool
all3 p = not . any (not . p)
-- works

-- all4 :: (a -> Bool) -> [a] -> Bool
-- all4 p = map p . and
-- type error, and return Bool, map needs [Bool]

all5 :: (a -> Bool) -> [a] -> Bool
all5 p xs = foldl (&&) True (map p xs)
-- works

all6 :: (a -> Bool) -> [a] -> Bool
all6 p xs = foldr (&&) False (map p xs)
-- fails, all [] = True

all7 :: (a -> Bool) -> [a] -> Bool
all7 p = foldr (&&) True . map p
-- works

-- e2 --
-- any0 :: (a -> Bool) -> [a] -> Bool
-- any0 p = map p . or
-- type error, or returns Bool, map needs [Bool]

any1 :: (a -> Bool) -> [a] -> Bool
any1 p = or . map p

any2 :: (a -> Bool) -> [a] -> Bool
any2 p xs = length (filter p xs) > 0

any3 :: (a -> Bool) -> [a] -> Bool
any3 p = not . null . dropWhile (not . p)

any4 :: (a -> Bool) -> [a] -> Bool
any4 p = null . filter p
-- fails, not . any4 would work

any5 :: (a -> Bool) -> [a] -> Bool
any5 p xs = not (all (\ x -> not (p x)) xs)

any6 :: (a -> Bool) -> [a] -> Bool
any6 p xs = foldr (\ x acc -> (p x) || acc) False xs

any7 :: (a -> Bool) -> [a] -> Bool
any7 p xs = foldr (||) True (map p xs)
-- fails, any [] = False

-- e3 --
takeWhile0 :: (a -> Bool) -> [a] -> [a]
takeWhile0 _ [] = []
takeWhile0 p (x : xs)
    | p x = x : takeWhile0 p xs
    | otherwise = takeWhile0 p xs
-- fails, keeps going

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x : xs)
    | p x = x : takeWhile1 p xs
    | otherwise = []
-- works

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p (x : xs)
    | p x = takeWhile2 p xs
    | otherwise = []
-- fails, doesn't cons anything

takeWhile3 :: (a -> Bool) -> [a] -> [a]
takeWhile3 p = foldl (\ acc x -> if p x then x : acc else acc) []
-- fails, builds the list backwards and doesn't stop for p x = False