import Lab3

-- e0 --
e0 = sum . evens $ [827305 .. 927104]

-- e1 --
e1 = sum . evens $ []

-- e2 --
e2 = sum . evens $ [1,3..]

-- e3 --
-- no code needed

-- e4 --
e4 = sumSquares 50

-- e5 --
e5 = sumSquares' 50

-- e6 --
e6 = sum $ squares' 10 0

-- e7 --
e7 = sum $ squares' 0 10

-- e8 --
e8 = foldr (-) 0 . map (uncurry (*)) $ coords 5 7

-- e9-12 --
-- done by inspection
