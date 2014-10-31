-- needed for e7
import Data.Char 

-- needed for e3
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- e0 --
-- sum100a = sum [[x * x] | x <- [1 .. 100]]
-- type error in sum

sum100b = sum [x ^ 2 | x <- [1 .. 100]]
-- works

sum100c = sum [const 2 x | x <- [1 .. 100]]
-- this is 2 + 2 + ... + 2 (100 times) = 200

sum100d = foldl (+) (1) [x ^ 2 | x <- [1 .. 100]]
-- this is 1 + 1^2 + 2^2 + ... + 100^2

-- e1 --
replicate0 n a = [True | _ <- [1..n]]
-- fails, replicates n Trues

replicate1 n a = [n | _ <- [1..n]]
-- fails, replicates n n times

replicate2 n a = [a | _ <- [1..a]]
-- fails, unless a = n

replicate3 n a = [a | _ <- [1..n]]
-- works

-- e2 --
pyths0 n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y],
    x^2 + y^2 == z^2]
-- fails, only checks z<=y<=x, but z>x,y

pyths1 n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n],
    x^2 + y^2 == z^2]
-- fails, ony checks x<=y<=y, which avoids x,y swaps but we're counting those

pyths2 n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n],
    x^2 + y^2 == z^2]
-- works

pyths3 n = [(x,y,(x^2 + y^2)) | x <- [1..n], y <- [1..n]]
-- fails, z = sqrt(x^2 + y^2) must be an integer

-- e3 --
perfects0 n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (factors num) == num
-- fails, factors include num itself

perfects1 n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (init (factors num)) == num
-- works

perfects2 n = [isPerfect x | x <- [1..n]]
    where isPerfect num = sum (init (factors num)) == num
-- fails, returns [Bool] showing if the position is perfect or not

-- perfects3 n = [x | x <- [1..n], isPerfect x]
--     where isPerfect num = init (factors num) == num
-- type error, can't compare a list with a number for equality

-- e4 --
e4 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

e40 = [z | z <- [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]]
-- fails, returns [[(Int,Int)]]

e41 = concat [[[(x,y)] | x <- [1,2,3]] | y <- [4,5,6]]
-- fails, returns [[(Int,Int)]] (deferently than e40)

-- e42 = concat [(x,y) | y <- [4,5,6]] | x <- [1,2,3]
-- parse error

e43 = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
-- works

-- e5 --
find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions0 :: (Eq a) => a -> [a] -> [Int]
positions0 x xs = find x (zip xs [0..n])
    where n = length xs - 1
-- works

-- positions1 :: (Eq a) => a -> [a] -> [Int]
-- positions1 x xs = find x xs
-- fails, wrong types passed to find

-- positions2 :: (Eq a) => a -> [a] -> [Int]
-- positions2 x xs = find x (zipWith (+) xs [0..n])
--     where n = length xs - 1
-- fails, zipWith fails as a isn't Num as (+) would need

-- positions3 :: (Eq a) => a -> [a] -> [Int]
-- positions3 x xs = find n (zip xs [0..x])
--     where n = length xs - 1
-- fails, a isn't Enum as needed for [0..x]

-- e6 --
scalarproduct0 :: [Int] -> [Int] -> Int
scalarproduct0 xs ys = sum [x * y | x <- xs, y <- ys]
-- fails, sum all possible products, not just corresponding ones

scalarproduct1 :: [Int] -> [Int] -> Int
scalarproduct1 xs ys = sum [x * y | (x,y) <- xs `zip` ys]
-- works

scalarproduct2 :: [Int] -> [Int] -> Int
scalarproduct2 xs ys = product (zipWith (+) xs ys)
-- fails, takes the product of corresponding sums

-- scalarproduct3 :: [Int] -> [Int] -> Int
-- scalarproduct3 xs ys = sum (product [(x,y) | x <- xs, y <- ys])
-- fails, product needs [Int] not [(Int,Int)]

-- e7 --
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
    | otherwise = c
   
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

e7 = encode 13 "Think like a Fundamentalist Code like a Hacker"

-- e8 --
e8 = [(x, y) | x <- [1, 2], y <- [1, 2]]

-- e9 --
e9 = [x | x <- [1, 2, 3], y <- [1..x]]

-- e10 --
e10 = sum [x | x <- [1..10], even x]

-- e11 --
e11 = 1 : [x + 1 | x <- e11]

-- e12 --
riffle0 :: [a] -> [a] -> [a]
riffle0 xs ys = concat [[x,y] | x <- xs, y <- ys]
-- fails, too many elements

riffle1 :: [a] -> [a] -> [a]
riffle1 xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]
-- works

-- riffle2 :: [a] -> [a] -> [a]
-- riffle2 xs ys = [x,y | (x,y) <- xs `zip` ys]
-- parse error

-- riffle3 :: [a] -> [a] -> [a]
-- riffle3 xs ys = [x : [y] | x <- xs, y <- ys]
-- type error

-- e13 --
divides :: Int -> Int -> Bool
divides n d = n `mod` d == 0

divisors0 :: Int -> [Int]
divisors0 x = [d | d <- [1..x], x `divides` d]
-- works

divisors1 :: Int -> [Int]
divisors1 x = [d | d <- [1..x], d `divides` x]
-- fails, arguments to divides are reversed

divisors2 :: Int -> [Int]
divisors2 x = [d | d <- [2..x], x `divides` d]
-- fails, skips 1 as divisor

-- divisors3 :: Int -> [Int]
-- divisors3 x = [d | d <- [1..x], x divides d]
-- type error, divides needs back quotes