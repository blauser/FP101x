-- e0 --
-- halve0 xs = (take n xs, drop n xs)
--   where n = length xs / 2
-- type error
  
halve1 xs = splitAt (length xs `div` 2) xs

halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
  
halve3 xs = splitAt (length xs `div` 2)
-- doesn't work, returns a function

halve4 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2
-- doesn't work, misses an element

halve5 xs = splitAt (div (length xs) 2) xs

-- halve6 xs = splitAt (length xs / 2) xs
-- type error


halve7 xs = (take n xs, drop n xs)
  where n = length xs `div` 2