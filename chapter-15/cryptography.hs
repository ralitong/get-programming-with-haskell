main = do
    print(rotN 4 L1)
    print(rotN 4 L2)
    print(rotN 4 L3)
    print(rotN 4 L4)
    -- Fun fact, since Bool type is a member of Enum and Bounded
    -- the rotN algorithm also works
    -- Basically you can use rotN if the type rotated is both a member of Enum and
    -- Bounded
    print(rotN 2 True)
    print(rotN 2 False)
    print("The original message: " ++ show message)
    print("Encrypted message:    " ++ show(fourLetterEncoder message))
    -- since we're using ROTN to rotate/encrypt our messages
    -- we can use the same rotate function to decrypt messages
    print("Decrypted message:    " ++ show(fourLetterEncoder (fourLetterEncoder message)))
    print(threeLetterMessage)
    print(threeLetterEncoder threeLetterMessage)
    print(threeLetterDecoder (threeLetterEncoder threeLetterMessage))
    print(rotEncoder "REVELUV")
    print(rotEncoder (rotEncoder "REVELUV"))


-- Reasons why Show, Enum and Bounded are derived
-- Show - for printing in main = do
-- Enum - to convert the the data constructors (L1 .. L2) to Int to easily do math
-- operations e.g. ROT13
-- Bounded - In order to determine how much you need to cycle
-- L4 = 3 then adding 1 (L4 + 1) should be 0 (L1)
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

-- How the algorithm works ...
-- Get the median of the alphabetSize
-- Convert the letter to its integer values hence `fromEnum c`
-- Add the median and integer value as offset
-- Mod the offset so that the rotation value is within the range of the alphbet
-- Finally convert the rotation value to a letter
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize


-- To rotate Char (Characters) we need to obtain the largest Char
-- We can use maxBound since Char is a member of the Bounded type
-- largest Char is the total amount of Chars
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

-- Since the lowestChar is 0 we need to add + 1 to the size of the alphabet
-- Reason for this
-- Ex. A = 0 , B = 1, C = 2, D = 3
-- D is the last letter so we use it as the size of the alphabet
-- If we rotate A = 0 and the median would be D = 3 / 2 = 1
-- rotating A = 0 + 1 , 1 which is B
-- rotating B back to A would B = 1 + 1 = 2
-- 2 is C, therefore we should add 0
rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- An example of encoding a message instead of single chars
message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

-- Using map function to rotate the chars in the message array (vals)
fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4l = rotN alphaSize

-- The problem with encoding/decoding alphabets with three letters
-- A, B, C say you want to rotate A
-- alphabet size = 3
-- Get half of the alphabet size 3 / 2 = 1 as offset
-- A = 0 , offset = 1 , A + offset = 1, which is B
-- if B is rotated then it should turn back to A
-- B + offset = 2, which is C
-- So the function below gives the wrong result if you use it as a decoder
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3l = rotN alphaSize

-- to solve the issue of odd alphabet sizes, we should add 1 to the offset
-- if the alphabet size is odd
-- The function below checks the alphabet size and adds 1 to the offset if
-- it detects that the alphabet size is odd
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else fromEnum c + halfN + 1
          rotation = offset `mod` n


threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
                          where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
                                rot3ldecoder = rotNdecoder alphaSize

-- Here is a function that rotate Strings
-- rotChar is already defined above
rotEncoder :: String -> String
rotEncoder text = map rotChar text

-- largestCharNumber = getting the maximum bound of Char
rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where alphaSize = largestCharNumber + 1
          rotCharDecoder = rotNdecoder alphaSize
