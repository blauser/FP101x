-- e0 --
-- done by inspection

-- for e1-6
testFunc :: Eq c => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
testFunc f g p xs = f p xs == g p xs

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

-- e4 --
dropWhile0 :: (a -> Bool) -> [a] -> [a]
dropWhile0 _ [] = []
dropWhile0 p (x : xs)
    | p x = dropWhile0 p xs
    | otherwise = x : xs
-- works

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x : xs)
    | p x = dropWhile1 p xs
    | otherwise = xs
-- fails, drops the first failure of p

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 p = foldr (\ x acc -> if p x then acc else x : acc) []
-- fails, continues dropping after first failure

dropWhile3 :: (a -> Bool) -> [a] -> [a]
dropWhile3 p = foldl add []
    where add [] x = if p x then [] else [x]
          add acc x = x : acc
-- fails, reverses order of dropWhile p xs

-- e5 --
map0 :: (a -> b) -> [a] -> [b]
map0 f = foldr (\ x xs -> xs ++ [f x]) []
-- fails, reverses mapped list

-- map1 :: (a -> b) -> [a] -> [b]
-- map1 f = foldr (\ x xs -> f x ++ xs) []
-- type error, f x is not a list, can't ++ it

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldl (\ xs x -> f x : xs) []
-- fails, reverses mapped list

map3 :: (a -> b) -> [a] -> [b]
map3 f = foldl (\ xs x -> xs ++ [f x]) []
-- works

-- e6 --
filter0 :: (a -> Bool) -> [a] -> [a]
filter0 p = foldl (\ xs x -> if p x then x : xs else xs) []
-- fails, filters but reverses

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\ x xs -> if p x then x : xs else xs) []
-- works

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\ x xs -> if p x then xs ++ [x] else xs) []
-- fails, filters but reverses

-- filter3 :: (a -> Bool) -> [a] -> [a]
-- filter3 p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []
-- type error, x is bound to a list and xs is bound to a item

-- e7 --
dec2int0 :: [Integer] -> Integer
dec2int0 = foldr (\ x y -> 10 * x + y) 0
-- fails, is equivalent to sum . map (*10)

dec2int1 :: [Integer] -> Integer
dec2int1 = foldl (\ x y -> x + 10 * y) 0
-- fails, is equivalent to sum . map (*10)

dec2int2 :: [Integer] -> Integer
dec2int2 = foldl (\ x y -> 10 * x + y) 0
-- works

dec2int3 :: [Integer] -> Integer
dec2int3 = foldr (\ x y -> x + 10 * y) 0
-- fails, builds number in reverse

-- e8-10 --
-- done by inspection

-- needed for e11-13 --
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

-- e11 --
type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

-- e12 --
map' f = unfold null (f . head) tail

-- e13 --
iterate' f = unfold (const False) id f

-- e14-25 --
-- done by inspection

-- e26-29 --
-- done with GHCi

-- e30-31 --
-- done by inspection
