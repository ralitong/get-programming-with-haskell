import Data.List
main = do
    print(fst author)
    print(snd author)
    print"Sorting by first names ..."
    print(sort names)
    print"Sorting by last names ..."
    print(sortBy compareLastNames names)
    print"Sorting names v2"
    print(sortBy compareLastNamesV2 names)


author = ("Will", "Kurt")
names = [
    ("Ian", "Curtis"),
    ("Bernard", "Summer"),
    ("Peter", "Hook"),
    ("Stephen", "Morris"),
    ("Anne", "Curtis")]


compareLastNames name1 name2 = if lastName1 > lastName2
                                then GT
                                else if lastName1 < lastName2
                                then LT
                                else if firstName1 > firstName2
                                then GT
                                else if firstName1 < firstName2
                                then LT
                                else EQ
                                where lastName1 = snd name1
                                      lastName2 = snd name2
                                      firstName1 = fst name1
                                      firstName2 = fst name2


compareLastNamesV2 name1 name2 = if   compare lastName1 lastName2 == EQ
                                 then compare firstName1 firstName2
                                 else compare lastName1 lastName2
                                 where lastName1 = snd name1
                                       lastName2 = snd name2
                                       firstName1 = fst name1
                                       firstName2 = fst name2