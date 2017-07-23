
let (--) i j =
  let rec aux n acc =
    if n < i
    then acc
    else aux (n - 1) (n :: acc)
  in aux j [] ;;

let max2 a b = if a > b then a else b

let rec max xs =
  match xs with
    [x] -> x
    | hd :: tl -> (max2 hd (max tl));;

let rec max_aux current_max xs =
  match xs with
    [] -> current_max
    | x :: xss -> max_aux (max2 current_max x) xss 

let maxx xs = max_aux (List.hd xs) (List.tl xs);;

let max3 xs = List.fold_left max2 (List.hd xs) (List.tl xs);;

print_int (max3 (1--10000000));;



