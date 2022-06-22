main = do
    print(myGCD 20 16)
    print(sayAmount 1)
    print(sayAmount 2)
    print(sayAmount 3)
    -- print(myHead [])
    print(myTail [1, 2, 3])
    -- print(myTail []) this will not work with print

-- myGCD a b = if remainder == 0
--             then b
--             else myGCD b remainder
--             where remainder = a `mod` b

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b) 
-- 2 4
-- 4 2
-- 2

sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

myHead (x:_) = x
myTail [] = []
myTail (_:xs) = xs