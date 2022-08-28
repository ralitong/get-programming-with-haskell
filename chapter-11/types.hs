main = do
  print (x)
  print (x * 2000)
  print (x ^ 2000)
  print (y)
  print (y * 2000)
  print (y ^ 2000)
  print (letter)
  print (interestRate)
  print (isFun)
  print (values)
  print (testScores)
  print (letters)
  print (letters == "abc")
  print (aPet)
  print (anotherPet)
  print (ageAndHeight)
  print (firstLastMiddle)
  print (streetAddress)
  print (double 4)
  print (half 5)
  print (halve 5)
  print (show 6)
  print (show 'c')
  print (show 6.0)
  print (printDouble 7)
  print (read "6" :: Int)
  print (read "6" :: Double)
  print (((makeAddressLambda 123) "Happy St") "Haskell Town")
  print (((makeAddress 123) "Happy St") "Haskell Town")
--   Desugared way of calling functions
  print (makeAddressLambda 123 "Happy St" "Haskell Town")
  print( head [1, 2, 3])


x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: [Char]
letters = ['a', 'b', 'c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Int -> Int
halve n = div n 2

printDouble :: Int -> String
printDouble n = show (double n)

z = read "6"

q = z / 2

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeAddressLambda =
  ( \number ->
      ( \street ->
          (\town -> (number, street, town))
      )
  )

ifEven :: (Int -> Int ) -> Int -> Int
ifEven f n = if even n
                then f n
                else n

simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

-- Type variable, simple anything
simple :: myType -> myType
simple myType = myType

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

-- f1 only returns a type with the same type used in the argument
-- f1 :: a -> a
-- f2 can return a type that differs from the argument
-- f2 :: a -> b

-- myFilter :: (a -> b -> c) -> [d] -> [d]

myTail :: [a] -> [a]
myTail (x:xs) = xs
myTail [] = []

-- Can't return a empty list on myHead
-- myHead :: [a] -> a
-- myHead (x:xs) = x
-- myHead [] = []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
                        where newInit = f init x