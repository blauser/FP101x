import Parsing

-- e0 --
e0 = parse item "hello"

-- e1 --
e1 = parse (return 1 +++ return 2)

-- e2 --
e2 = parse (return 1) "hello"

-- e3 --
e3 = parse (item +++ return 'a') "hello"

-- e4 --
p4 :: Parser (Char,Char)
p4 = do x <- item
        item
        y <- item
        return (x,y)

e4 = parse p4

-- e5 --
e5 = parse (char 'a' +++ return 'b')

-- e6 --
e6 = map (parse int) ["-007","1234","abc"]

-- e7 --
e7 = map (parse comment) ["nope","--Yes\n"]

-- e8 --
e8 = map (parse expr) ["1","5-2-1","10-9-0-0-0-0-0"]

