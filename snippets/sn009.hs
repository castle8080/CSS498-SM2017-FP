
my_max [x] = x

my_max (x : xs) =
  let xs_max = my_max xs
  in
    if x > xs_max
    then x
    else xs_max

