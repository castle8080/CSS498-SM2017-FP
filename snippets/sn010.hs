
my_max xs =
  let
    max_2 a b =
      if b > a
      then b
      else a
    max_acc current_max xs =
      case xs of
        []        -> current_max
        (x : xs') -> max_acc (max_2 current_max x) xs'
  in
    max_acc (head xs) (tail xs)


