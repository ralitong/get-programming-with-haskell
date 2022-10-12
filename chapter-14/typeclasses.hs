import Data.List
main = do
    print S1
    print S2
    print S3
    print S4
    print(S6 == S5)
    print(S1 > S2)
    print(AA < ZZ)
    print(AA > ZZ)
    print(AAA > ZZZ)
    print(AAA < ZZZ)
    print[S1 .. S6]
    print[S2,S4 .. S6]
    print[S4 .. S6]
    print[S1 ..]
    print(sort names)
    print rolledFiveSidedDie


-- data NewEngland = ME | VT | NH | MA | RI | CT
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 

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

-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _ = False

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



-- To use Eq e.g. comparison operators
-- < > <= >=
-- You only need to implement compare ::
-- compare returns 3 data constructors
-- LT, EQ and GT

-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT
--     compare S4 S4 = EQ

    -- This part is not really needed
    -- ** compare S4 _ = GT **
    -- Since there is compare S5 _ = GT
    -- and S6 _ = GT

    -- compare _ S4 = LT

-- A much better way to implement Ordering and Equality
-- is to derive
data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)

-- This SixSided Die Ordering and Eq implemented with Eq and Ord
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord)

-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq)


-- A much much better way to implement Ordering and Eq is using Enums
-- You only need to implement `toEnum` and `fromEnum`
-- but if you use this to print [S1 .. ] 
-- This will error out since it can only support to S5
-- -- [S1 ..] will print up to infinity I think ? :D
-- instance Enum SixSidedDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"
--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5

-- You could just derive SixSidedDie with Enum
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Eq, Ord)

-- Defining types for complex Data Structures like tuples
-- type Name = (String, String)
-- names :: [Name]
-- names = [ ("Emil", "Cioran"),("Eugene", "Thacker"),("Friedrich", "Nietzsche")]

-- If you want to sort the list above by last name then 
-- `sort names` isn't gonna work
-- It will just result to [("Emil","Cioran"),("Eugene","Thacker"),("Friedrich","Nietzsche")]

-- You could implement Ord but this will not work since Haskell already has an
-- sort implementation of type (String, String)
-- instance Ord Name where
--     compare (f1, l1) (f2,l2) = compare (l1, f1) (l2, f2)

-- You can however implement your own tuple type using Data Constructor
data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2,l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("Emil","Cioran")
        ,Name ("Eugene", "Thacker")
        ,Name ("Friedrich", "Nietzsche")]

-- Now if your print `sort names` it will now print the names list
-- by last name
-- [("Emil","Cioran"),("Eugene","Thacker"),("Friedrich","Nietzsche")]

-- Here's an example to easily implement Eq and Ord if SixSidedDie is
-- already deriving Enum
-- The key is using `fromEnum` to convert the Enums to Int
-- making it easier to implement Eq and Ord since Haskell already
-- has Eq and Ord implementations for Int

-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

-- instance Eq SixSidedDie where
--     (==) a b = fromEnum a == fromEnum b

-- instance Ord SixSidedDie where
--     compare a b = compare (fromEnum a) (fromEnum b)


class (Eq a, Enum a) => Die a where
  roll :: Int -> a

data FiveSidedDie = T1 | T2 | T3 | T4 | T5  deriving (Enum, Eq, Show)

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)

rolledFiveSidedDie :: FiveSidedDie
rolledFiveSidedDie = roll 10