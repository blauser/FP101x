-- e0 --
e0 = [False, True, False, True]
-- [Bool]

-- e1 --
e1 = [[1,2], [3,4]]
-- [[Integer]], [[Int]], Num t => [[t]]

-- e2 --
e2 = [[[1, 2, 3]], [[3, 4, 5]]]
-- [[[Integer]]], [[[Int]]], Num t => [[[t]]]

-- e3 --
e3 x = x * 2
-- Num a => a -> a

-- e4 --
e4 (x, y) = x
-- (a, b) -> a

-- e5 --
e5 (x, y, z) = z
-- (a, b, c) -> c

-- e6 --
e6 x y = x * y
-- Num a => a -> a -> a

-- e7 --
e7 (x, y) = (y, x)
-- (a, b) = (b, a)

-- e8 --
e8 x y = (y, x)
-- a -> b -> (b, a)

-- e9 --
e9 [x, y] = (x, True)
-- [t] -> (t, Bool)

-- e10 --
e10 (x, y) = [x, y]
-- (t, t) -> [t]

-- e11 --
e11 :: (Char, Bool)
e11 = ('\a', True)

-- e12 --
e12 :: [(Char, Int)]
e12 = [('a', 1)]

-- e13 --
e13 :: Int -> Int -> Int
e13 x y = x + y * y
-- why not e13 x y = x * x - x ? because e13 x True would work

-- e14 --
e14 :: ([Char], [Float])
e14 = ("Haskell", [3.1, 3.14, 3.141, 3.1415])

-- e15 --
e15 :: [a] -> [b] -> (a, b)
e15 xs ys = (head xs, head ys)