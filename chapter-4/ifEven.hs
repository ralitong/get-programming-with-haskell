main = do
    print(ifEven (\x -> x*2) 6)
    -- quick check 4.1 - my custome cubing function
    print(ifEven (\x -> x*3) 6)


ifEven myFunction x = if even x
                        then myFunction x
                        else x

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSqaure n = ifEven square n
