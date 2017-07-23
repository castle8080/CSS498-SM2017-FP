

my_max [x] = x

my_max (x : xs) =
  if x > (my_max xs)
  then x
  else (my_max xs)

main = do
  putStrLn $ show (my_max [0..100])
 
