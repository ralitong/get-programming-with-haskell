main = do
    print(S1)
    print(S2)
    print(S3)
    print(S4)
    print(S5)
    print(S6)
    -- print(One)


-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6  deriving (Show)
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

-- Incorrect implementation show for SixSidedDie
-- show :: SixSidedDie -> String
-- show S1 = "one"
-- show S2 = "two"
-- show S3 = "three"
-- show S4 = "four"
-- show S5 = "five"
-- show S6 = "six"

data TwoSidedDie = One | Two

show :: TwoSidedDie -> String
show One = "one"
show Two = "two"