main = do
    print(doSomethingWithTwo inc)
    print(doSomethingWithThree inc)


ifEven myFunction x = if even x
                        then myFunction x
                        else x

inc n = n + 1

-- sample of a closure
-- inc is captured in genIfEven
ifEvenInc n = genIfEven inc

genIfEven f = (\x -> ifEven f x)

genIfXEven x = (\f -> ifEven f x)

doSomethingWithTwo = genIfXEven 2
doSomethingWithThree = genIfXEven 3

