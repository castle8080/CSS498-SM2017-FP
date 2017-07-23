

reduce operator [x] =
  x

reduce operator (x1:x2:xs) =
  reduce operator ((operator x1 x2) : xs) 

add a b = a + b

my_sum xs =
  reduce add xs

my_sum2 xs =
  reduce (+) xs

my_product xs =
  reduce (*) xs


