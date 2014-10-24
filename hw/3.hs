-- e0 --
-- halve0 xs = (take n xs, drop n xs)
--   where n = length xs / 2
-- type error
  
halve1 xs = splitAt (length xs `div` 2) xs
-- works

halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
-- works
  
halve3 xs = splitAt (length xs `div` 2)
-- doesn't work, returns a function

halve4 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2
-- doesn't work, misses an element

halve5 xs = splitAt (div (length xs) 2) xs
-- works

-- halve6 xs = splitAt (length xs / 2) xs
-- type error

halve7 xs = (take n xs, drop n xs)
  where n = length xs `div` 2
-- works
  
-- e1 --
safetail0 xs = if null xs then [] else tail xs
-- works

safetail1 [] = []
safetail1 (_ : xs) = xs
-- works

safetail2 (_ : xs)
  | null xs = []
  | otherwise = tail xs
-- fails on [] and doesn't work as tail otherwise

safetail3 xs
  | null xs = []
  | otherwise = tail xs
-- works
  
-- safetail4 xs = tail xs
-- safetail4 [] = []
-- fails on [] (and causes warning)

safetail5 [] = []
safetail5 xs = tail xs
-- works

safetail6 [x] = [x]
safetail6 (_ : xs) = xs
-- fails on []

safetail7
  = \ xs ->
    case xs of
      [] -> []
      (_ : xs) -> xs
-- works

-- e2 --
ortest :: (Bool -> Bool -> Bool) -> Bool
ortest f = and [f True True, f True False, f False True, not (f False False)]

False `or0` False = False
_ `or0` _ = True
-- works

False `or1` b = b
True `or1` _ = True
-- works

b `or2` c
  | b == c = True
  | otherwise = False
-- fails

b `or3` c
  | b == c = b
  | otherwise = True
-- works

b `or4` False = b
_ `or4` True = True
-- works

b `or5` c
  | b == c = c
  | otherwise = True
-- works

-- b `or6` True = b
-- _ `or6` True = True
-- throws exception (and causes warning)

False `or7` False = False
False `or7` True = True
True `or7` False = True
True `or7` True = True
-- works

-- e3 --
andtest :: (Bool -> Bool -> Bool) -> Bool
andtest f = and [f True True, not (f True False), not (f False True), not (f False False)]

True `and0` True = True
_ `and0` _ = False
-- works

a `and1` b = if a then if b then True else False else False
-- works

a `and2` b = if not (a) then not (b) else True
-- fails

-- a `and3` b = if a then b
-- not a proper expression

a `and4` b = if a then if b then False else True else False
-- fails

a `and5` b = if a then b else False
-- works

a `and6` b = if b then a else False
-- works

-- e4 --
mult0 x y z = \ x -> (\ y -> (\ z -> x * y * z))
-- fails as mult, works as (mult0 _ _ _) x y z

-- mult1 = \ x -> (x * \ y -> (y * \ z -> z))
-- type failure

mult2 = \ x -> (\ y -> (\ z -> x * y * z))
-- works

-- mult3 = ((((\x -> \y) -> \z) -> x * y) * z)
-- parsing error

-- e5 --
-- just syntax

-- e6 --
-- just syntax

-- e7 --
remove0 n xs = take n xs ++ drop n xs
-- fails, returns list

remove1 n xs = drop n xs ++ take n xs
-- fails, reorders list about nth place

remove2 n xs = take (n + 1) xs ++ drop n xs
-- fails, adds an extra copy of nth element

remove3 n xs = take n xs ++ drop (n + 1) xs
-- works

-- e8 --
funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs
-- [1,2,3,4,4,5,6,7]