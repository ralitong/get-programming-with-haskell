main = do
  print (patientInfo ("John", "Doe") 43 74)
  print (patientInfo ("Jane", "Smith") 25 62)
  print (firstName testPatient)
  print (lastName testPatient)
  print (canDonateTo patient1BT patient2BT)
  print (canDonateTo patient2BT patient1BT)
  print (canDonateTo patient1BT patient3BT)
  print (canDonateTo patient3BT patient1BT)
  print (showName name1)
  print (showName name2)
  print (height jackieSmith)
  print (showBloodType (bloodType jackieSmith))
  print (showName (name jackieSmith))
  print (patientCanDonateTo jackieSmith johnDoe)
  print (patientCanDonateTo johnDoe jackieSmith)
  putStr (patientSummary jackieSmith)
  putStr (patientSummary johnDoe)

testPatient = ("John", "Doe")

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

type Age = Int

type Height = Int

type Weight = Int

type PatientName = (String, String)

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patient age height = name ++ " " ++ ageHeight
  where
    name = lastName patient ++ ", " ++ firstName patient
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False -- otherwise

-- data Patient = Patient Name Sex Age Height Weight BloodType

patientCanDonateTo :: Patient -> Patient -> Bool
patientCanDonateTo p1 p2 = canDonateTo (bloodType p1) (bloodType p2) 


data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeSmith :: Patient
janeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 28 62 140 (BloodType O Neg)

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

jackieSmithUpdated :: Patient
jackieSmithUpdated = jackieSmith {age = 44}

patientSummary :: Patient -> String
patientSummary patient = 
    "**************\n" ++
    "Patient Name: " ++ showName (name patient) ++ "\n" ++
    "Sex: " ++ showSex (sex patient) ++ "\n" ++
    "Age: " ++ show (age patient) ++ "\n" ++
    "Height: " ++ show (height patient) ++ " in.\n" ++
    "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
    "BloodType: " ++ showBloodType (bloodType patient) ++ "\n" ++
    "**************\n"
