main = do
    print((add4 3) 2 3 4)
    print((add4 3) 5 6 7)
    print((add4 2 3) 1 2)
    print((add4 2 3) 4 5)


add4 a b c d = a + b + c + d

addXto3 x = (\b c d -> add4 x b c d)

addXYto2 x y = (\c d -> add4 x y c d)