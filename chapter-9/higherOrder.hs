import Data.Char (toLower)
main = do
  print (map reverse ["dog", "cat", "moose"])
  print (map head ["dog", "cat", "moose"])
  print (map (take 4) ["pumpkin", "pie", "peanut butter"])
  print (map ("a " ++) ["train", "plane", "boat"])
  print (addAnA ["train", "plane", "boat"])
  print (squareAll [1, 2, 3, 4])
  print (filter even [1, 2, 3, 4])
  print (filter (\word -> head word == 'a') ["apple", "banana", "avocado"])
  print(foldl (+) 0 [1, 2, 3, 4])
  print(concatAll ["h", "e", "l", "l", "o"])
  print(myProduct [1, 2, 3, 4])
  print(sumOfSquares [1, 2, 3, 4])
  print(myReverse [1, 2, 3, 4])
  print(foldl (+) 0 [1, 2, 3, 4])
  print(foldr (+) 0 [1, 2, 3, 4])
  print(foldl (-) 0 [1, 2, 3, 4])
  print(foldr (-) 0 [1, 2, 3, 4])
  print(foldr (-) 0 [1])
  print(foldr (-) 0 [1, 2])
  print(myElem 20 [1, 2, 3])
  print(myElem 2 [1, 2, 3])
  print(myPalindrome "A man a plan a canal Panama")
  print(harmonic 4)

addAnA [] = []
addAnA (x : xs) = ("a " ++ x) : addAnA xs

squareAll [] = []
squareAll (x : xs) = (x ^ 2) : squareAll xs

myMap f [] = []
myMap f (x : xs) = (f x) : myMap f xs

myFilter test [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

myRemove test [] = []
myRemove test (x : xs) =
  if test x
    then myRemove test xs
    else x : myRemove test xs

concatAll xs = foldl (++) "" xs
myProduct xs = foldl (*) 1 xs
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
                        where newInit = f init x

myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
                        where rightResult = myFoldr f init xs


myElem e l = (length (filter (\x -> x == e ) l ) ) > 0
myPalindrome word = filter (\c -> not(c == ' ' )) (map toLower word) == reverse (filter (\c -> not(c == ' ' )) (map toLower word))
harmonic n = foldr (+) 0 (map (\x -> 1 / x) [1 .. n])
-- right fold
-- myFoldr - 0 [1, 2]
-- (-) 1 ( (-) 2 )
-- (-) 1 ( (-) 2 [] )
-- (-) 1 2
-- -1

-- left fold
-- 0 [1, 2, 3, 4]
-- (-) ( - 0 1 ) [2, 3, 4]
-- (-) -1 [2, 3, 4]
-- (-) ( - -1 2) [3, 4]
-- (-) -3 [3, 4]
-- (-) (- -3 3) [4]
-- (-) -6 [4]
-- (-) (- 6 4) []
-- (-) -10 []
-- -10

