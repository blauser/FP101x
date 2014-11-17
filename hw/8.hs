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

