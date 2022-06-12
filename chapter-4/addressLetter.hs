main = do
    print(addressLetter ("Bob", "Smith") "PO Box 1234 - San Francisco, CA, 94111")
    print(addressLetterV2 ("Bob", "Smith") "ny")
    print(addressLetterV2 ("Bob", "Jones") "ny")
    print(addressLetterV2 ("Samantha", "Smith") "sf")
    print(addressLetterV2 ("Bob", "Smith") "reno")
    print(addressLetterV2 ("Bob", "Smith") "la")
    print(addressLetterV2 ("Stephen", "Curry") "dc")

addressLetter name location = nameText ++ " - " ++ location
                            where nameText = (fst name) ++ " " ++ (snd name)


sfOffice name = if lastName < "L"
                then nameText
                    ++ " - P0 Box 1234 - San Francisco, CA, 94111"
                else nameText
                    ++ " - P0 Box 1010 - San Francisco, CA, 94109"
                where lastName = snd name
                      nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
                where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
                    where nameText = snd name

dcOffice name = nameText ++ " - PO Box 555 - Washington, DC, 15000"
                where nameText = (fst name) ++ " " ++ (snd name) ++ " Esq"


getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetterV2 name location = locationFunction name
        where locationFunction = getLocationFunction location