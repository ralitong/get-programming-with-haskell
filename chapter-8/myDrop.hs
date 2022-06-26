main = do
  print (mydrop 2 [1, 2, 3, 4])
  print (myLength [1, 2, 3])
  print (myTake 3 [1, 2, 3, 4, 5])
  print (ackermann 3 3)
  print (ackermann 3 8)
  print (ackermann 3 9)
  print (collatz 9)
  print (collatz 999)
  print (collatz 92)
  print (collatz 91)
  print (map collatz [100 .. 120])
  print (myReverse [1, 2, 3, 4, 5])
  print (fib 3)
  print (fib 4)
  print (fastFib 1 1 3)
  print (fastFib 1 1 4)

mydrop 0 list = list
mydrop n list = mydrop (n - 1) (tail list)

myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

finiteCycle (first : rest) = first : rest ++ [first]

myCycle (first : rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n -1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n-2)

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n2 (counter - 1)