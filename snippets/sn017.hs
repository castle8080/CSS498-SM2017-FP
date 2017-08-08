import Data.List.Split
import Data.List

make_greeter format =
  let parts = splitOn "#{name}" format 
      greeter name = intercalate name parts 
  in greeter




