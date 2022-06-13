main = do
    print(subtractTwo 2)
    print(subtractTwo 4)

minus x y = x - y
inverse = flip minus
subtractTwo = inverse 2
