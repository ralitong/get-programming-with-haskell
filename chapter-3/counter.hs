main = do
    print (counter 1)

-- counter x = let x = x + 1
--                 in
--                     let x = x + 1
--                         in
--                             x

counter x =  (\x -> x + 1)  ((\x -> x + 1) ((\x -> x) x))