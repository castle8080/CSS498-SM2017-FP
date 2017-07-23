import qualified Data.Map as Map

double x = x + x
square x = x * x 
same x = x
zero x = 0

adjust_salaries salaries day =
  let operation =
        case day of
          "Friday"   -> square
          "Monday"   -> zero
          "Thursday" -> double
          _          -> same
  in
    [operation salary | salary <- salaries] 

apply_twice operation x =
  operation (operation x)

apply_all [] x = x
apply_all (op:ops) x = apply_all ops (op x)

operations_by_name = Map.fromList
  [ ("square", square),
    ("double", double),
    ("zero", zero) ]

apply_operation name value =
  (Map.findWithDefault same name operations_by_name) value 


get_operation "Friday"   = square
get_operation "Monday"   = zero
get_operation "Thursday" = double
get_operation _          = same


