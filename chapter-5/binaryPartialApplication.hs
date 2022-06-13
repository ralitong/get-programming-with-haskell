main = do
    print(addFour 2)

binaryPartialApplication binaryFunction = \x -> binaryFunction x
myAdder x y = x + y
partialAdder = binaryPartialApplication myAdder
addFour = partialAdder 4

