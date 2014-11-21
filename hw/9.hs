{-# LANGUAGE NPlusKPatterns #-}

import Data.List
import Data.Char
import Unsafe.Coerce
import Test.QuickCheck

data Nat = Zero
         | Succ Nat
         deriving Show

-- QuickCheck setup --
-- maybe an Arbitrary Nat?

-- e0 --
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1
-- selected solution for outside use

natToInteger0 :: Nat -> Integer
natToInteger0 Zero = 0
natToInteger0 (Succ n) = natToInteger0 n + 1

natToInteger1 :: Nat -> Integer
natToInteger1 (Succ n) = natToInteger1 n + 1
natToInteger1 Zero = 0

natToInteger2 :: Nat -> Integer
natToInteger2 n = natToInteger2 n
-- fails, infinite recursion

natToInteger3 :: Nat -> Integer
natToInteger3 (Succ n) = 1 + natToInteger3 n
natToInteger3 Zero = 0

natToInteger4 :: Nat -> Integer
natToInteger4 Zero = 1
natToInteger4 (Succ n) = (1 + natToInteger4 n) - 1
-- fails, always returns 1

natToInteger5 :: Nat -> Integer
natToInteger5 = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger6 :: Nat -> Integer
natToInteger6 = \ n -> genericLength [c | c <- show n, c == 'S']

{-
natToInteger7 :: Nat -> Integer
natToInteger7 = \ n -> length [c | c <- show n, c == 'S']
-- type error, length returns Int not Integer -}

-- e1 --
integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)
-- selected solution for outside use

integerToNat0 :: Integer -> Nat
integerToNat0 0 = Zero
integerToNat0 (n+1) = Succ (integerToNat0 n)

integerToNat1 :: Integer -> Nat
integerToNat1 0 = Succ Zero
integerToNat1 n = (Succ (integerToNat1 n))
-- fails, infinite recursion

{-
integerToNat2 :: Integer -> Nat
integerToNat2 n = product [(unsafeCoerce c) :: Integer | c <- show n]
-- type error, product [Integer] is an Integer not a Nat -}

integerToNat3 :: Integer -> Nat
integerToNat3 n = integerToNat3 n
-- fails, infinite recursion

integerToNat4 :: Integer -> Nat
integerToNat4 (n+1) = Succ (integerToNat4 n)
integerToNat4 0 = Zero

integerToNat5 :: Integer -> Nat
integerToNat5 (n+1) = let m = integerToNat5 n in Succ m
integerToNat5 0 = Zero

{-
integerToNat6 :: Integer -> Nat
integerToNat6 = head . m
    where {
          ; m 0 = [0]
          ; m (n + 1) = [sum [x | x <- (1 : m n)]]
          }
-- type error, m returns [Integer] not [Nat] -}

{-
integerToNat7 :: Integer -> Nat
integerToNat7 = \ n -> genericLength [c | c <- show n, isDigit c]
-- type error, genericLength doesn't return a Nat -}

-- e2 --
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
-- solution for outside use

add0 :: Nat -> Nat -> Nat
add0 Zero n = n
add0 (Succ m) n = Succ (add0 n m)

add1 :: Nat -> Nat -> Nat
add1 (Succ m) n = Succ (add1 n m)
add1 Zero n = n

add2 :: Nat -> Nat -> Nat
add2 Zero n = Zero
add2 (Succ m) n = Succ (add2 m n)
-- fails, 0 + 1 = 0

add3 :: Nat -> Nat -> Nat
add3 (Succ m) n = Succ (add3 m n)
add3 Zero n = Zero
-- fails, 0 + 1 = 0

add4 :: Nat -> Nat -> Nat
add4 n Zero = Zero
add4 n (Succ m) = Succ (add4 n m)
-- fails 1 + 0 = 0

add5 :: Nat -> Nat -> Nat
add5 n (Succ m) = Succ (add5 n m)
add5 n Zero = Zero
-- fails 1 + 0 = 0

add6 :: Nat -> Nat -> Nat
add6 n Zero = n
add6 n (Succ m) = Succ (add6 m n)

add7 :: Nat -> Nat -> Nat
add7 n (Succ m) = Succ (add7 m n)
add7 n Zero = n

-- QuickCheck testing --
prop_add add' (NonNegative n) (NonNegative m) =
    natToInteger (add' n' m') == natToInteger n' + natToInteger m'
        where n' = integerToNat n
              m' = integerToNat m

-- e3 --
mult0 :: Nat -> Nat -> Nat
mult0 Zero Zero = Zero
mult0 m (Succ n) = add m (mult0 m n)
-- fails, (Succ Zero) Zero doesn't match any pattern

mult1 :: Nat -> Nat -> Nat
mult1 m Zero = Zero
mult1 m (Succ n) = add m (mult1 m n)

mult2 :: Nat -> Nat -> Nat
mult2 m Zero = Zero
mult2 m (Succ n) = add n (mult2 m n)
-- fails, 0 * 2 = 1

mult3 :: Nat -> Nat -> Nat
mult3 m Zero = Zero
mult3 m n = add m (mult3 m (Succ n))
-- fails, infinite recursion

-- QuickCheck testing --
prop_mult mult' (NonNegative n) (NonNegative m) =
    natToInteger (mult' n' m') == natToInteger n' * natToInteger m'
        where n' = integerToNat n
              m' = integerToNat m

-- e4 --
data NTree = NLeaf Integer
           | NNode NTree Integer NTree

occurs0 :: Integer -> NTree -> Bool
occurs0 m (NLeaf n) = m == n
occurs0 m (NNode l n r)
    = case compare m n of
        LT -> occurs0 m l
        EQ -> True
        GT -> occurs0 m r

occurs1 :: Integer -> NTree -> Bool
occurs1 m (NLeaf n) = m == n
occurs1 m (NNode l n r)
    = case compare m n of
        LT -> occurs1 m r
        EQ -> True
        GT -> occurs1 m l
-- fails, recurses down wrong branches

{-
occurs2 :: Integer -> NTree -> Bool
occurs2 m (NLeaf n) = compare m n
occurs2 m (NNode l n r)
    = case compare m n of
        LT -> occurs2 m l
        EQ -> True
        GT -> occurs2 m r
-- type error, compare returns Ordering not Bool -}

occurs3 :: Integer -> NTree -> Bool
occurs3 m (NLeaf n) = m == n
occurs3 m (NNode l n r)
    = case compare m n of
        LT -> occurs3 m l
        EQ -> False
        GT -> occurs3 m r
-- fails, value in a NNode is missed

occurs4 :: Integer -> NTree -> Bool
occurs4 m (NLeaf n) = m == n
occurs4 m (NNode l n r)
    | m == n = True
    | m < n = occurs4 m l
    | otherwise = occurs4 m r

occurs5 :: Integer -> NTree -> Bool
occurs5 m (NLeaf n) = m == n
occurs5 m (NNode l n r)
    | m == n = True
    | m > n = occurs5 m l
    | otherwise = occurs5 m r
-- fails, recurses down the wrong branches

{-
occurs6 :: Integer -> NTree -> Bool
occurs6 m n = m == n
occurs6 m (NNode l n r)
    | m == n = True
    | m < n = occurs6 m l
    | otherwise = occurs6 m r
-- type error, n should be NLeaf n -}

{-
occurs7 :: Integer -> NTree -> Bool
occurs7 m n = m == n
occurs7 m (NNode l n r)
    | m == n = False
    | m < n = occurs7 m r
    | otherwise = occurs7 m l
-- type error, n should be NLeaf n -}

-- e5 --
data Tree = Leaf Integer
          | Node Tree Tree

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r)
    = abs (leaves l - leaves r) <= 1 && balanced l && balanced r
    where leaves (Leaf _) = 1
          leaves (Node l r) = leaves l + leaves r

-- e6 --
halve xs = splitAt (length xs `div` 2) xs

balance :: [Integer] -> Tree
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs

-- e7-10 --
-- done by inspection

-- e11 --
class Monoid a where
    mempty :: a
    (<>) :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>) = (++)

-- e12 --
-- done by inspection

-- e13 --
class (Functor f) => Foldable f where
    fold :: (Monoid m) => f m -> m
    
instance Foldable [] where
    fold = foldr (<>) mempty
