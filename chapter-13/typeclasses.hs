main = do
  print (True)
  print (False)
  print (Chocolate)
  print (Vanilla == Vanilla)
  print (Chocolate == Vanilla)
  print (Chocolate /= Vanilla)
  print (Chocolate > Vanilla)
  -- print (cycleSucc 'a')
  -- print (cycleSucc 1)
  -- print(inc myInt)
  print(minBound :: Int)


simple x = x

aList = ["cat", "dog", "mouse"]

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

data IceCream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- inc :: Int -> Int
-- inc x = x  + 1

-- TODO
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n

myInt :: Int
myInt = 9223372036854775807