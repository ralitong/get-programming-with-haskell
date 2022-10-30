main = do
  print (rotN 4 L1)
  print (rotN 4 L2)
  print (rotN 4 L3)
  print (rotN 4 L4)
  -- Fun fact, since Bool type is a member of Enum and Bounded
  -- the rotN algorithm also works
  -- Basically you can use rotN if the type rotated is both a member of Enum and
  -- Bounded
  print (rotN 2 True)
  print (rotN 2 False)
  print ("The original message: " ++ show message)
  print ("Encrypted message:    " ++ show (fourLetterEncoder message))
  -- since we're using ROTN to rotate/encrypt our messages
  -- we can use the same rotate function to decrypt messages
  print ("Decrypted message:    " ++ show (fourLetterEncoder (fourLetterEncoder message)))
  print (threeLetterMessage)
  print (threeLetterEncoder threeLetterMessage)
  print (threeLetterDecoder (threeLetterEncoder threeLetterMessage))
  print (rotEncoder "REVELUV")
  print (rotEncoder (rotEncoder "REVELUV"))
  print (intToBits 32)
  print (bitsToInt (intToBits 32))
  print (bitsToInt (intToBits maxBound))
  print(bitsToChar (charToBits 'R'))
  print(bitsToChar (charToBits 'V'))
  print(applyOTP myPad myPlainText)
  print(encoderDecoder "book")
  -- The param below is the result of `encoderDecoder "book"`
  print(encoderDecoder "1\a\a\ETX")
  print(encode Rot "Haskell")
  print(decode Rot "\557128\557153\557171\557163\557157\557164\557164")
  print(encode myOTP "Learn Haskell")
  print(decode myOTP "Ldcqj%Nf{bog`")
  print(encode myOTP "this is a longer sentence, I hope it encodes")
  print(encode myOTP "tikp$lu'i)fdbjk}0bw}`pxt}5:R<uqoE\SOHKW\EOT@HDGMOX")
  print(examplePRNG 12345)
  print(examplePRNG 72)
  print(examplePRNG 71)
  print(examplePRNG 34)
  

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
  where
    halfAlphabet = alphabetSize `div` 2
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
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- An example of encoding a message instead of single chars
message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

-- Using map function to rotate the chars in the message array (vals)
fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
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
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l = rotN alphaSize

-- to solve the issue of odd alphabet sizes, we should add 1 to the offset
-- if the alphabet size is odd
-- The function below checks the alphabet size and adds 1 to the offset if
-- it detects that the alphabet size is odd
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else fromEnum c + halfN + 1
    rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize

-- Here is a function that rotate Strings
-- rotChar is already defined above
rotEncoder :: String -> String
rotEncoder text = map rotChar text

-- largestCharNumber = getting the maximum bound of Char
rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where
    alphaSize = largestCharNumber + 1
    rotCharDecoder = rotNdecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

-- This is used to easily XOR two boolean values
xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
-- zip takes makes a tuple for each element of two lists ^__^
-- [A, B, C, D]
-- [1, 2, 3, 4]
-- [(A,1),(B,2),(C,3),(D,4)]
xor list1 list2 = map xorPair (zip list1 list2)

-- To makes things more readable, we'll create a type synonym for lists of Bool
-- to Bits
type Bits = [Bool]

-- Converting an Int to bits are for example
-- Let's convert the number 3
-- 3 mod 2 = 1, 3 / 2 = 1
-- 1 mod 2 = 1, 1 / 2 = 0
-- The remainders are 3 in bit representation 11
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

-- The function above produces the correct result but in reverse
-- intToBits' 2 = [False,True]
-- intToBits' 8 = [False,False,False,True]

-- The function below will reverse the bits produced by intToBits'
-- and prepend False values so that all resulting Bits will have the same length
intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    -- cycle [False] will endlessly loop the list [False]
    -- so it'd be like [False,False ... False]
    -- take will get n elements endless [False] list
    leadingFalses = take missingBits (cycle [False])

-- How do we decide the length of Bits?
-- Since we convert Char to Int to convert them to Bits
-- We take the highest possible value of Int (maxBound :: Int)
-- and then take the length of the highest possible value
-- The length of the maxBits = 63
maxBits :: Int
maxBits = length (intToBits' maxBound)

-- You can now convert a Char to Bits
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

-- To convert bits back to Int
-- Lets convert 11
-- The formula is the sum of all bit * 2 ^ index
-- 11 = (1 * 2 ^ 0) + (1 * 2 ^ 1)
-- 11 = (1 * 1) + (1 * 2)
-- 11 = (1) + (2)
-- 11 = 1 + 2
-- 11 = 3

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where
    size = length bits
    -- This gets the indices
    -- If the bits input is 100 then indices is [3-1, 3-2, 3-3]
    -- [2, 1, 0]
    indices = [size -1, size -2 .. 0]
    -- Since we are are only interested in bits that have true values
    -- because 0 * 2 ^ index will always result to 0
    -- We use filter to filter out bits that False values
    -- Ex. 100
    -- (1 * 2 ^ 2) + (0 * 2 ^ 1) + (0 * 2 ^ 0)
    -- We filter out the 2 remaining 0 bits to the left
    trueLocations =
      filter
        (\x -> fst x == True)
        (zip bits indices)

-- You can now then convert the Bits to a Char
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


-- The one time pad
-- This works by having two Strings
-- The input and pad
-- You XOR each character in input to each pad's character
-- input[n] XOR pad[n]
-- Ideally, the pad should have the same length as the input
myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

-- This function only returns the bits of the XOR'ed plaintext and pad
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair))
                          (zip padBits plaintextBits)
          where padBits = map charToBits pad
                plaintextBits = map charToBits plaintext

-- This function will now return XOR'ed pad and plaintext to a String
applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where bitList = applyOTP' pad plaintext

-- Using partial applications, you can create a decoder and encoder using the 
-- same one-time pad function
-- Note:
-- Partial applications are functions with incomplete parameters
-- myCompleteFunc a b -> a + b
-- myPartialFuncThatAdds20 = myCompleteFunc 20

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

-- The Cipher class to generalize cipher operations
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

-- A dataType to define a Cipher instance for ROT13 algorithm
data Rot = Rot

-- The dataType Rot serves as a reference on what algorithm the Cipher class
-- will use e.g.
-- encode Rot "Haskell"
instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

-- Unlike ROT13 which only takes one input, one-time pad takes an input String
-- and a pad
-- It is possible to add a parameter for the one-time pad data type
data OneTimePad = OTP String

-- Notice in the encode & decode part the `pad` part is also passed
-- (OTP pad)
instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

-- Unlike Rot datatype which has no parameter
-- You need to an instance of the OneTimePad datatype
-- A cycle of infinite characters should be enough so that the
-- length of the pad is greater than the message to encode
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- A pseudo-random number generator PRNG that creates a stream of
-- random numbers to be used to XOR the bits for one-time pad
-- a & b is used to determine the randomness
-- maxNumber is the bound of many numbers to generate
-- seed - a random number also to effect randomness
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
-- This is a partial function since the seed value is not included in the parameter
examplePRNG = prng 1337 7 100
