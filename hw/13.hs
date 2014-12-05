-- It's been really fun.

foldl0 :: (a -> b -> a) -> a -> [b] -> a
foldl0 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a

foldl1' :: (a -> b -> a) -> a -> [b] -> a
foldl1' f a bs = foldr (\a b -> f b a) a bs

foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f = flip $ foldr (\a b g -> b (f g a)) id

foldl3 :: (a -> b -> a) -> a -> [b] -> a
foldl3 = foldr . flip