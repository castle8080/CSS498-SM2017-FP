{-
  Name: Bryan Castillo
  Exercises: Chapter 6 (Recursive functions) "Programming in Haskell"
-}
import Data.List (all)
import Control.Exception
import GHC.Exception
import Data.Maybe

-- Utility function for checking all items in a list are true.
all_true xs = all id xs

{-
  1. How does the recursive version of the factorial function behave if
     applied to a negative argument, such as (-1)? Modify the definition
     to prohibit negative arguments by adding a guard to the recursive case.
-}

{-
   The factorial from the book:

     fac :: Int -> Int
     fac 0 = 1
     fac n = n * fac (n-1)

   Answer:

   The version from the book will recurse infinitely when a negative number
   is passed to fac.

   The definition below does not recurse infinitely. It will throw an error:

   Example:

    *Main> fac (-10)
    *** Exception: src\chapter6.hs:(32,1)-(34,25): Non-exhaustive patterns in function fac

   TODO: How do I catch the error for testing?
-}
fac :: Int -> Int
fac 0 = 1
fac n
  | n > 0 = n * fac (n-1)

{-
  2. Define a recursive function sumdown :: Int -> Int that returns the sum of
     the non-negative integers from a given value down to zero. For example,
     sumdown 3 should return the result 3+2+1+0 = 6.
-}

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n
  | n > 0 = n + (sumdown $ n - 1)

test2 =
  all_true [
    (sumdown 10) == sum [1..10],
    (sumdown 1) == 1,
    (sumdown 2) == 3
  ]

{-
  3. Define the exponentiation operator ^ for non-negative integers using the
     same pattern of recursion as the multiplication operator *, and show how
     the expression 2 ^ 3 is evaluated using your definition.

     How it is evaluated:

     2 `pow` 3
     { applying pow }
     2 * (2 `pow` (3 - 1))
     { applying - }
     2 * (2 `pow` 2)
     { applying pow }
     2 * (2 * (2 `pow` (2 - 1)))
     { applying - }
     2 * (2 * (2 `pow` 1))
     { applying pow }
     2 * (2 * (2 * (2 `pow` (1 - 0))))
     { applying - }
     2 * (2 * (2 * (2 `pow` 0)))
     { applying pow }
     2 * (2 * (2 * 1))
     { applying * }
     8
-}

pow :: Int -> Int -> Int
n `pow` 0 = 1
n `pow` p
  | p > 0 = n * (n `pow` (p - 1))

test3 =
  all_true [
    (2 `pow` 10) == 1024,
    (9 `pow` 0) == 1,
    (7 `pow` 1) == 7
  ]

{-
  4. Define a recursive function euclid :: Int -> Int -> Int that implements
     Euclid's algorithm for calculating the greatest common divisor of two
     non-negative integers: if the two numbers are equal, this number is the
     result; otherwise, the smaller number is subtracted from the larger, and
     the same process is then repeated. For example:

       > euclid 6 27
       3
-}

euclid :: Int -> Int -> Int
euclid a b
  | a < 0 || b < 0 = error "Invalid number."
  | a == b = a
  | a < b = euclid a (b - a)
  | a > b = euclid (a - b) b

test4 =
  (euclid 6 27) == 3

{-
  5. Using the recursive definitions given in this chapter, show how
     length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.
-}

{-
  Answer:

    length from book:

    length [] = 0
    length (_:xs) = 1 + length xs

      Evaluate
      ------------------
      length [1,2,3]
      { apply length }
      1 + length [2,3]
      { apply length }
      1 + 1 + length [3]
      { apply length }
      1 + 1 + 1 + length []
      { apply length }
      1 + 1 + 1 + 0
      { apply + }
      3

    drop 0 xs = xs
    drop _ [] = []
    drop n (_:xs) = drop (n-1) xs

      Evaluate
      ------------------
      drop 3 [1,2,3,4,5]
      { apply drop }
      drop (3 - 1) [2,3,4,5]
      { apply - }
      drop 2 [2,3,4,5]
      { apply drop }
      drop (2 - 1) [3,4,5]
      { apply - }
      drop 1 [3,4,5]
      { apply drop }
      drop (1 - 1) [4,5]
      { apply - }
      drop 0 [4,5]
      { apply drop }
      [4,5]


    init [_] = []
    init (x:xs) = x : init xs

      Evaluate
      ------------------
      init [1,2,3]
      { apply init }
      1 : init [2,3]
      { apply init }
      1 : 2 : init [3]
      { apply init }
      1 : 2 : []
      { apply : }
      [1,2]
-}

{-
  6. Without looking at the definitions from the standard prelude, define the
     following library functions on lists using recursion.

     a. Decide if all logical values in a list are True:
       and :: [Bool] -> Bool

     b. Concatenate a list of lists:
       concat :: [[a]] -> a

     c. Produce a list with n identical elements:
       replicate :: Int -> a -> [a]

     d. Select the nth element of a list:
       (!!) :: [a] -> Int -> a

     e. Decide if a value is an element of a list:
       elem :: Eq a => a -> [a] -> Bool

     Note: most of these functions are defined in the prelude using other
     library functions rather than using explicit recursion, and are generic
     functions rather than being specific to the type of lists.
-}

my_and :: [Bool] -> Bool
my_and [] = True
my_and (b:bs) = b && (my_and bs)

my_concat :: [[a]] -> [a]
my_concat [] = []
my_concat (x:xs) = x ++ (my_concat xs)

my_replicate :: Int -> a -> [a]
my_replicate 0 x = []
my_replicate n x
  | n > 0 = x : my_replicate (n -1) x

my_nth :: [a] -> Int -> a
my_nth (x:xs) 0 = x
my_nth (x:xs) n
  | n > 0 = my_nth xs (n - 1)

my_elem :: Eq a => a -> [a] -> Bool
my_elem _ [] = False
my_elem w (x:xs) = (w == x) || (my_elem w xs)

test6 =
  all_true [
    my_and [],
    my_and [True],
    my_and [True, True],
    not $ my_and [False],
    not $ my_and [True, False],
    not $ my_and [False, True],
    my_concat [[1]] == [1],
    my_concat [[1,2],[3]] == [1,2,3],
    my_replicate 0 'a' == "",
    my_replicate 10 'a' == "aaaaaaaaaa",
    my_nth "bryan" 0 == 'b',
    my_nth "bryan" 4 == 'n',
    my_elem 'o' "bob",
    my_elem 'b' "bob",
    my_elem 'z' "xyz",
    not $ my_elem 'j' "bob",
    not $ my_elem 'z' ""
  ]

{-
  7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that
     merges two sorted lists to give a single sorted list. For example:

       > merge [2,5,6] [1,3,4]
       [1,2,3,4,5,6]

     Note: your definition should not use other functions on sorted lists such
     as insert or isort, but should be defined using explicit recursion.
-}

my_merge :: Ord a => [a] -> [a] -> [a]
my_merge [] ys = ys
my_merge xs [] = xs
my_merge (x:xs) (y:ys)
  | x < y     = x : my_merge xs (y:ys)
  | otherwise = y : my_merge (x:xs) ys

test7 =
  all_true [
    my_merge [2,5,6] [1,3,4] == [1,2,3,4,5,6],
    my_merge [1,2] [] == [1,2],
    my_merge [] [1,2] == [1,2]
  ]

{-
  8. Using merge, define a function msort :: Ord a => [a] -> [a] that
     implements merge sort, in which th eempty list and singleton lists are
     already sorted, and any other list is sorted by merging together the two
     lists that result from sorting the two halves of the list separately.

     Hint: first define a function halve :: [a] -> ([a],[a]) that splits a
     list into two halves whose lengths differ by at most one.
-}

halve xs = (take hl xs, drop hl xs)
  where hl = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = my_merge (msort left) (msort right)
  where (left,right) = halve xs

test8 =
  all_true [
    msort ([] :: [Int]) == [],
    msort [1] == [1],
    msort [9,3,1,6,4] == [1,3,4,6,9],
    msort [4,3,2,1] == [1,2,3,4],
    msort [1,2,1,2] == [1,1,2,2]
  ]

{-
  9. Using the five-step process, construct the library functions that:

    a. calculates the sum of a list of numbers.
    b. take a given number of elements form the start of a list;
    c. select the last element of a non-empty list.
-}

my_sum :: Num a => [a] -> a
my_sum [] = 0
my_sum (x:xs) = x + (my_sum xs)

my_take :: Int -> [a] -> [a]
my_take 0 _ = []
my_take n (x:xs)
  | n > 0 = x : my_take (n - 1) xs

my_last :: [a] -> a
my_last [x] = x
my_last (x:xs) = my_last xs

test9 =
  all_true [
    my_sum [] == 0,
    my_sum [1..3] == sum [1..3],
    my_take 0 [1] == [],
    my_take 1 [1,2] == [1],
    my_take 3 [1..10] == [1,2,3],
    my_last [1] == 1,
    my_last [1..10] == 10
  ]

main = do
  putStrLn $ "Running chapter 6 tests:"
  putStrLn $ "test2: " ++ (show test2)
  putStrLn $ "test3: " ++ (show test3)
  putStrLn $ "test4: " ++ (show test4)
  putStrLn $ "test6: " ++ (show test6)
  putStrLn $ "test7: " ++ (show test7)
  putStrLn $ "test8: " ++ (show test8)
  putStrLn $ "test9: " ++ (show test9)
