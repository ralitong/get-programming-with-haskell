main = do
    print(head [1, 2, 3])
    print(head [[1, 2], [3, 4], [5, 6]])
    print(tail [1, 2, 3])
    print(tail [3])
    -- using cons
    print(1:[])
    print(1:2:3:4:[])
    print((1,2):(3,4):(5,6):[])
    print(1:[2,3,4])
    -- cons and strings
    print(['h','e','l','l','o'])
    print('h':'e':'l':'l':'o':[])
    -- will not work
    -- print("h":"ello")
    -- print(['h']:['e','l','l','o'])
    -- print('h':[]:'e':'l':'l':'o':[])
    -- this will work
    print("h" ++ "ello")
    print([1] ++ [2, 3, 4])
    -- generating some data
    print([1 .. 10])
    -- this generates odd numbers 
    print([1,3 .. 10])
    print([1, 1.5 .. 5])
    print([1,0 .. -10])
    -- this generates a never ending list, commented this out on purpose
    -- print([1 ..])
    -- print(stillLongList)
    -- this will run
    -- print(reverse [1..])
    -- access by index
    print([1, 2, 3] !! 0)
    print("puppies" !! 4)
    -- out of bounds error
    -- print([1 .. 10] !! 11)
    -- index as prefix
    print((!!) [1, 2, 3] 0)
    -- partial functions on the right
    print(paExample1 2)
    print(paExample2 2)
    -- partial functions on the left
    print(paExample3 "dog")
    -- length
    print(length [1 .. 20])
    print(length [(10, 20), (1, 2), (15, 16)])
    print(length "quicksand")
    -- reverse
    print(reverse [1, 2, 3])
    print(reverse "cheese")
    -- palindrome
    print(isPalindrome "cheese")
    print(isPalindrome "racecar")
    print(isPalindrome [1, 2, 3])
    print(isPalindrome [1, 2, 1])
    -- checking if elem is inside list
    print(elem 13 [0, 13 .. 100])
    print(elem 'p' "cheese")
    print(respond "hello")
    print(respond "hello!")
    -- take and drop
    print(take 5 [2, 4 .. 100])
    print(take 3 "wonderful")
    print(take 5 (reverse [2, 4 .. 100]))
    print(drop 2 [1, 2, 3, 4 ,5])
    print(drop 5 "very awesome")
    -- zip
    print(zip [1, 2, 3] [2, 4, 6])
    print(zip "dog" "rabbit")
    print(zip ['a' .. 'f'] [1 ..])
    -- cycle
    print(ones 2)
    print(ones 4)
    print(assignToGroups 3 ["file1.txt","file2.txt","file3.txt"
                            ,"file4.txt","file5.txt","file6.txt","file7.txt"
                            ,"file8.txt"])
    print(myRepeat "china" 5)
    print(subseq 2 5 [1 .. 10])
    print(subseq 2 7 "a puppy")
    print(inHalf 4 [1 .. 10])
    print(inHalf 6 [1 .. 10])



simple x = x
longList = [1 .. ]
stillLongList = simple longList
paExample1 = (!!) "dog"
paExample2 = ("dog" !!)
paExample3 = (!! 2)
isPalindrome word = word == reverse word
respond :: Foldable t => t Char -> [Char]
respond phrase = if '!' `elem` phrase
                then "wow!"
                else "uh.. okay"
ones n = take n (cycle [1])
assignToGroups n aList = zip groups aList
                        where groups = cycle [1 .. n]
myRepeat input times = take times (cycle [input])
subseq start end mylist = take (end - start) (drop start mylist)
half myList = take (div (length myList) 2) myList
inHalf input myList = input `elem` half myList