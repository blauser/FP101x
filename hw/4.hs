-- e0 --
-- sum100a = sum [[x * x] | x <- [1 .. 100]]
-- type error in sum

sum100b = sum [x ^ 2 | x <- [1 .. 100]]
-- works

sum100c = sum [const 2 x | x <- [1 .. 100]]
-- this is 2 + 2 + ... + 2 (100 times) = 200

sum100d = foldl (+) (1) [x ^ 2 | x <- [1 .. 100]]
-- this is 1 + 1^2 + 2^2 + ... + 100^2