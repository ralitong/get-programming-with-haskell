main = do
    print(S1)
    print(S2)
    print(S3)
    print(S4)
    print(S6 == S6)


-- data NewEngland = ME | VT | NH | MA | RI | CT

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 
instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"


-- Haskell compiler is smart enough that if you define
-- `==` cases for the SixSidedDie data constructors e.g. (S1, S2 ... etc)
-- it automatically figures out the inverse `/=` (not equal)
-- but ... this will only work if you define `==` for all data constructors
instance Eq SixSidedDie where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = False

-- This will compile fine but this will not print
-- because SixSidedDie by has `show` implementation
-- by default

-- show :: SixSidedDie -> String
-- show S1 = "I"
-- show S2 = "II"
-- show S3 = "III"
-- show S4 = "IV"
-- show S5 = "V"
-- show S6 = "VI"

-- Another example on the same problem above
-- data TwoSidedDie = One | Two
-- show :: TwoSidedDie -> String
-- show One = "one"
-- show Two = "two"

