-- e0 --
-- no code needed

-- e1 --
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

-- e2 --
putStrLn0 :: String -> IO ()
putStrLn0 [] = putChar '\n'
putStrLn0 xs = putStr' xs >> putStrLn0 ""

putStrLn1 :: String -> IO ()
putStrLn1 [] = putChar '\n'
putStrLn1 xs = putStr' xs >> putChar '\n'

putStrLn2 :: String -> IO ()
putStrLn2 [] = putChar '\n'
putStrLn2 xs = putStr' xs >>= \ x -> putChar '\n'

-- putStrLn3 :: String -> IO ()
-- putStrLn3 [] = putChar '\n'
-- putStrLn3 xs = putStr' xs >> \ x -> putChar '\n'
-- type failure with the lambda function, (>>) returns ()

putStrLn4 :: String -> IO ()
putStrLn4 [] = putChar '\n'
putStrLn4 xs = putStr' xs >> putStr' "\n"

putStrLn5 :: String -> IO ()
putStrLn5 [] = putChar '\n'
putStrLn5 xs = putStr' xs >> putStrLn5 "\n"
-- fails, infinite recursion

-- putStrLn6 :: String -> IO ()
-- putStrLn6 [] = return ""
-- putStrLn6 xs = putStrLn6 xs >> putStr' "\n"
-- type failure, return "" gives String not ()

-- putStrLn7 :: String -> IO ()
-- putStrLn7 [] = putChar "\n"
-- putStrLn7 xs = putStr' xs >> putChar '\n'
-- type failur, putChar takes Char not String

-- e3 --
getLine0 :: IO String
getLine0 = get0 ""

get0 :: String -> IO String
get0 xs = do x <- getChar
             case x of
                ' ' -> return xs
                '\n' -> return xs
                _ -> get0 (xs ++ [x])

getLine1 :: IO String
getLine1 = get1 ""

get1 :: String -> IO String
get1 xs = do x <- getChar
             case x of
                '\n' -> return xs
                _ -> get1 (x : xs)

getLine2 :: IO String
getLine2 = get2 []

get2 :: String -> IO String
get2 xs = do x <- getChar
             case x of
                '\n' -> return xs
                _ -> get2 (xs ++ [x])

getLine3 :: IO String
getLine3 = get3 []

get3 :: String -> IO String
get3 xs = do x <- getChar
             case x of
                '\n' -> return (x : xs)
                _ -> get3 (xs ++ [x])

-- e4 --
-- done by inspection

-- e5 --
-- sequence_0 :: Monad m => [m a] -> m ()
-- sequence_0 [] = return []
-- sequence_0 (m : ms) = m >> \ _ -> sequence_0 ms
-- type failure, return [] is not m () but m []

sequence_1 :: Monad m => [m a] -> m ()
sequence_1 [] = return ()
sequence_1 (m : ms) = (foldl (>>) m ms) >> return ()
-- works

-- sequence_2 :: Monad m => [m a] -> m ()
-- sequence_2 ms = foldl (>>) (return ()) ms
-- type error, foldl expects [m ()] based on (return ())

sequence_3 :: Monad m => [m a] -> m ()
sequence_3 [] = return ()
sequence_3 (m : ms) = m >> sequence_3 ms
-- works

sequence_4 :: Monad m => [m a] -> m ()
sequence_4 [] = return ()
sequence_4 (m : ms) = m >>= \ _ -> sequence_4 ms
-- works

-- sequence_5 :: Monad m => [m a] -> m ()
-- sequence_5 ms = foldr (>>=) (return ()) ms
-- type failure, (>>=) expects a value to act upon

sequence_6 :: Monad m => [m a] -> m ()
sequence_6 ms = foldr (>>) (return ()) ms
-- works

-- sequence_7 :: Monad m => [m a] -> m ()
-- sequence_7 ms = foldr (>>) (return []) ms
-- type error, return [] needs to be return ()

-- e6 --
sequence0 :: Monad m => [m a] -> m [a]
sequence0 [] = return []
sequence0 (m : ms) = m >>= \ a -> do as <- sequence0 ms
                                     return (a : as)

-- sequence1 :: Monad m => [m a] -> m [a]
-- sequence1 ms = foldr func1 (return ()) ms
--     where
--         func1 :: (Monad m) => m a -> m [a] -> m [a]
--         func1 m acc = do x <- m
--                          xs <- acc
--                          return (x : xs)
-- type error, return () is of type m () not of type m [a]

-- sequence2 :: Monad m => [m a] -> m [a]
-- sequence2 ms = foldr func2 (return []) ms
--     where
--         func2 :: (Monad m) => m a -> m [a] -> m [a]
--         func2 m acc = m : acc
-- fails, monad not invoked in func2

-- sequence3 :: Monad m => [m a] -> m [a]
-- sequence3 [] = return []
-- sequence3 (m : ms) = return (a : as)
--     where
--         a <- m
--         as <- sequence3 ms
-- fails, no do for do notation

sequence4 :: Monad m => [m a] -> m [a]
sequence4 ms = foldr func4 (return []) ms
    where
        func4 :: (Monad m) => m a -> m [a] -> m [a]
        func4 m acc = do x <- m
                         xs <- acc
                         return (x : xs)

-- sequence5 :: Monad m => [m a] -> m [a]
-- sequence5 [] = return []
-- sequence5 (m : ms) = m >> \ a ->
--                             do as <- sequence5 ms
--                                return (a : as)
-- type error, (>>) doesn't return a value for the lambda expression to use

-- sequence6 :: Monad m => [m a] -> m [a]
-- sequence6 [] = return []
-- sequence6 (m : ms) = m >>= \a ->
--     as <- sequence6 ms
--     return (a : as)
-- fails, no do for do notation

sequence7 :: Monad m => [m a] -> m [a]
sequence7 [] = return []
sequence7 (m : ms) = do a <- m
                        as <- sequence7 ms
                        return (a : as)

-- e7 --
mapM0 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM0 f as = sequence (map f as)

mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1 f [] = return []
mapM1 f (a : as) = f a >>= \ b -> mapM1 f as >>= \ bs -> return (b : bs)

-- mapM2 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM2 f as = sequence_ (map f as)
-- type error, sequence_ discards values leaving () not [b]

-- mapM3 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM3 f [] = return []
-- mapM3 f (a : as) = f a >>= \ b -> mapM3 f as >> \ bs -> return (b : bs)
-- type error, (>>) returns () but the lambda expersion expects a value

-- mapM4 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM4 f [] = return []
-- mapM4 f (a : as) = do 
--                       f a -> b
--                       mapM4 f as -> bs
--                       return (b : bs)
-- fails, arrows pointing the wrong way in do notation

mapM5 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM5 f [] = return []
mapM5 f (a : as) = do b <- f a
                      bs <- mapM5 f as
                      return (b : bs)

mapM6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM6 f [] = return []
mapM6 f (a : as) = f a >>= \ b -> do bs <- mapM6 f as
                                     return (b : bs)

-- mapM7 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM7 f [] = return []
-- mapM7 f (a : as) = f a >>= \ b -> do bs <- mapM7 f as
--                                      return (bs ++ [b])
-- fails, resulting list is reversed

-- e8 --
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs)
    = do flag <- p x
         ys <- filterM' p xs
         if flag then return (x : ys) else return ys

p8 :: Int -> IO Bool
p8 n = do putStrLn . show $ n
          return (even n)

-- e9 --
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = f a x >>= \ a' -> foldLeftM f a' xs

e9 = foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

-- e10 --
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (x:xs) = foldRightM f b xs >>= \ b' -> f x b'

e10 = foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r

-- e11 --
liftM0 :: Monad m => (a -> b) -> m a -> m b
liftM0 f m = do x <- m
                return (f x)

-- liftM1 :: Monad m => (a -> b) -> m a -> m b
-- liftM1 f m = m >>= \ a -> f a
-- type error, f a is of type b not m b

liftM2 :: Monad m => (a -> b) -> m a -> m b
liftM2 f m = m >>= \ a -> return (f a)

-- liftM3 :: Monad m => (a -> b) -> m a -> m b
-- liftM3 f m = return (f m)
-- type error, m is of type m a, but f needs type a

liftM4 :: Monad m => (a -> b) -> m a -> m b
liftM4 f m = m >>= \ a -> m >>= \ b -> return (f a)
-- fails, m a is bound twice, so double side-effects

liftM5 :: Monad m => (a -> b) -> m a -> m b
liftM5 f m = m >>= \ a -> m >>= \ b -> return (f b)
-- fails, m a is bound twice, so double side-effects

-- liftM6 :: Monad m => (a -> b) -> m a -> m b
-- liftM6 f m = mapM f [m]
-- type error, mapM needs (a -> m b) but f is (a -> b)

-- liftM7 :: Monad m => (a -> b) -> m a -> m b
-- liftM7 f m = m >> \ a -> return (f a)
-- type error, (>>) doesn't pass along a value to the lambda expression

t11 :: a -> IO a
t11 x = do putStrLn "IO is happening!"
           return x
