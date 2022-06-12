x = 4

add1 y = y + x
add2 y = (\x -> y + x) 3
add3 y = (\y -> (\x -> y + x ) 1 ) 2