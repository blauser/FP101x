import Lab4
import Data.Char

-- e0 --
e0 = triangle 500

-- e1 --
e1 = triangle (-500)

-- e2 --
-- e2 = triangle pi
-- fails type check

-- e3 --
e3 = count 722 ys

-- e4 --
e4 = count 101 (poem >>= \x -> map (ord . \x -> chr (ord x + 4)) x)

-- e5 --
e5 = euclid (13404, 8832)

-- e6 --
e6 = euclid (1, 0)

-- e7 --
e7 = sum $ funkyMap (+10) (+100) ys
e7' = sum $ funkyMap (\c -> if c == 'e' then 1 else 0) ord (poem >>= id)

-- e8-20 --
-- done by inspection/GHCi
