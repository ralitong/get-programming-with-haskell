main = do
    print(getOz coffeeCup)
    print(getOz afterASip)
    print(getOz afterTwoSips)
    print(getOz afterGulp)
    print(getOz afterBigGulp)
    print(getOz afterManySips)
    


-- cup flOz = flOz
cup flOz = \message -> message flOz
aCup = cup 6
coffeeCup = cup 12
getOz aCup = aCup (\flOz -> flOz)
drink aCup ozDrank = if ozDiff >= 0
                    then cup ozDiff
                    else cup 0
    where flOz = getOz aCup
          ozDiff = flOz - ozDrank

afterASip = drink coffeeCup 1
afterTwoSips = drink afterASip 1
afterGulp = drink afterTwoSips 4
afterBigGulp = drink coffeeCup 20
isEmpty aCup = getOz aCup == 0
afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]
