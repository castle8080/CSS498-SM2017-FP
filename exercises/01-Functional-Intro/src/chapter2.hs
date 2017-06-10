{-
  Name: Bryan Castillo
  Exercises: Chapter 2 "Programming in Haskell"
-}
import Data.List

 {-
  2. Parenthesise the following numeric expressions:
    2^3*4
    2*3+4*5
    2+3*4^5
 -}

test2 =
  all id [
    2^3*4   == (2^3)*4,
    2*3+4*5 == ((2*3)+(4*5)),
    2+3*4^5 == 2+(3*(4^5))
  ]

{-
  3. The script below contains three syntactic errors. Correct these errors.

        N = a `div` length xs
            where
                a = 10
               xs = [1,2,3,4,5]

    Answer:
      1. The indentation of a and xs was not correct (conforming to the
          layout rule).
      2. The name of the function should be lower case.
-}

n = a `div` length xs
  where
    a  = 10
    xs = [1,2,3,4,5]

{-
  4. The library function last selects the last element of a non-empty list;
     for example, last [1,2,3,4,5] = 5. Show how the function last could be
     defined in terms of the other library functions introduced in this
     chapter. Can you think of another possible definition?

   Answer:
     1. The first one uses the length and the !! operator.
        This seems to iterate the list twice though.

     2. Another way to do it is with recursion. This should iterate the
        list once.
-}

my_last1 xs = xs !! ((length xs) - 1)

my_last2 [] = error "empty list"
my_last2 (x:[]) = x
my_last2 (x:xs) = my_last2 xs

test4 =
  all id [
    my_last1 [1] == 1,
    my_last1 [1,3,2] == 2,
    my_last2 [1] == 1,
    my_last2 [1,3,2] == 2
  ]

{-
  5. The library function init removes the last element from a non-empty list;
     for example, init [1,2,3,4,5] = [1,2,3,4,5]. Show how init could
     similarly defined in two different ways.
-}

my_init1 xs = take ((length xs) - 1) xs

my_init2 [] = error "empty list"
my_init2 ([x]) = []
my_init2 (x:xs) = x : my_init2 xs

test5 =
  all id [
    my_init1 [1] == [],
    my_init1 [1,3,2] == [1,3],
    my_init2 [1] == [],
    my_init2 [1,3,2] == [1,3]
  ]

main = do
  putStrLn "Running chapter2 tests:"
  putStrLn $ "Test2: " ++ (show test2)
  putStrLn $ "Test4: " ++ (show test4)
  putStrLn $ "Test5: " ++ (show test5)
