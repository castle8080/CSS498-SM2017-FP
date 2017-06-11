{-
  Name: Bryan Castillo
  Exercises: Chapter 5 (List comprehensions) "Programming in Haskell"
-}
import Data.List (all)
import Data.Char

-- Utility function for checking all items in a list are true.
all_true xs = all id xs

{-
  1. Using a list comprehension, give an expression that calculates the sum
     1^2 + 2^2 + ... 100^2 of the first one hundrend integeger squares.
-}

sum_squares_to_100 = sum [x^2 | x <- [1..100]]

{-
  2. Suppose that a coordinate grid of size m x n is given by the list of all
     pairs (x,y) of integers such that 0 <= x <= m and 0 <= y <= n. Using a List
     comprehension, define a function:
       grid :: Int -> Int -> [(Int,Int)]
     that returns a coordinate grid of a given size. For example:

       > grid 1 2
       [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(row,col) | row <- [0..m], col <- [0..n]]

test2 =
  (grid 1 2) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

{-
  3. Using a list comprehension and the function grid above, define a function
       square :: Int -> [(Int, Int)]
     that returns a coordinate square of size n, excluding the diagonal from
     (0,0) to (n,n). For example:

       > square 2
       [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
-}

square n = [(row,col) | (row,col) <- grid n n, row /= col]

test3 =
  (square 2) == [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

{-
  4. In a similar way to the function length, show how the library function
      replicate :: Int -> a -> [a]
     that produces a list of identical elements can be defined using a list
     comprehension, For example:

       > replicate 3 True
       [True,True,True]
-}

my_replicate :: Int -> a -> [a]
my_replicate n item = [item | _ <- [1..n]]

test4 = (my_replicate 3 True) == [True,True,True]

{-
  5. A triple (x,y,z) of positive integers is Pythagorean if it satisfies the
     equation x^2 + y^2 = z^2. Using a list comprehension with three generators,
     define a function pyths :: Int -> [(Int,Int,Int)] that returns the list of
     all such triples whose components are at most a given limit. For example:

       > pyths 10
       [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}

pyths limit =
  [(x,y,z) | x <- [1..limit], y <- [1..limit], z <- [1..limit], x^2 + y^2 == z^2]

test5 =
  (pyths 10) == [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

{-
  6. A positive integer is perfect if it equals the sum of all of its factors,
     excluding the number itself. Using a list comprehension and the function
     factors, define a function:
       perfects :: Int -> [Int]
     that returns the list of all perfect numbers up to a given limit.
     For example:

       > perfects 500
       [6,28,496]
-}

-- factors from book
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool
perfect n = (sum [x | x <- factors n, x /= n]) == n

perfects :: Int -> [Int]
perfects limit = [x | x <- [1..limit], perfect x]

test6 =
  (perfects 500) == [6,28,496]

{-
  7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]]
     with two generators can be re-expressed using two comprehensions with
     single generators. Hint: nest one comprehension within the other and make
     use of the library function concat :: [[a]] -> [a].
-}

test7 =
  (concat [ [ (x,y) | y <- [3,4]] | x <- [1,2]]) ==
    [(x,y) | x <- [1,2], y <- [3,4]]

{-
  8. Redefine the function positions using the function find.
-}

-- from book
find k t = [v | (k',v) <- t, k == k']

-- version from book
-- positions x xs = [i | (x'i) <- zip xs [0..], x == x']

-- my version using find
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

test8 =
  all_true [
    (positions 1 [1,2,1]) == [0,2],
    (positions 2 [1,2,1]) == [1],
    (positions 0 [1,2,1]) == []
  ]

{-
  9. The scalar product of two lists of integers xs and ys of length n is
     given by the sum of the products of corresponding integers:

        n-1
        +---
        \
         >  (xsi * ysi)
        /
        +---
        i=0
     In a similar manner to chisqr, show how a list comprehension can be used to
     define a function:
       scalaproduct :: [Int] -> [Int] -> Int
     that returns the scalar product of two lists. For example:

     > scalarproduct [1,2,3] [4,5,6]
     32
-}

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys =
  sum [ x * y | (x,y) <- zip xs ys]

test9 =
  (scalarproduct [1,2,3] [4,5,6]) == 32

{-
  10. Modify the Caesar cipher program to also handle upper-case letters.

    Code from book (reformatted):
      let2int :: Char -> Int
      let2int c = ord c - ord 'a'

      int2let :: Int -> Char
      int2let n = chr (ord 'a' + n)

      shift :: Int -> Char -> Char
      shift n c
        | isLower c =
          int2let ((let2int c + n) `mod` 26)
        | otherwise =
          c

      encode :: Int -> String -> String
      encode n xs = [shift n x | x <- xs]
-}

let2int sl c = ord c - ord sl
int2let sl n = chr (ord sl + n)

-- lower case version
let2int_l = let2int 'a'
int2let_l = int2let 'a'

-- upper case version
let2int_u = let2int 'A'
int2let_u = int2let 'A'

shift n c
  | isLower c =
    int2let_l $ (let2int_l c + n) `mod` 26
  | isUpper c =
    int2let_u $ (let2int_u c + n) `mod` 26
  | otherwise =
    c

encode n xs = [shift n x | x <- xs]

test_encode n s =
  (encode (-n) $ encode n s) == s

test10 =
  all_true [
    test_encode 0 "I like Haskel!",
    test_encode 25 "LISP IS INTERESTING",
    test_encode 13 "JavaScript is.... evIL!."
  ]

-- Main driver
main = do
  putStrLn $ "Running chapter 5 tests:"
  putStrLn $ "test1: sum of squares to 100: " ++ (show sum_squares_to_100)
  putStrLn $ "test2: " ++ (show test2)
  putStrLn $ "test3: " ++ (show test3)
  putStrLn $ "test4: " ++ (show test4)
  putStrLn $ "test5: " ++ (show test5)
  putStrLn $ "test6: " ++ (show test6)
  putStrLn $ "test7: " ++ (show test7)
  putStrLn $ "test8: " ++ (show test8)
  putStrLn $ "test9: " ++ (show test9)
  putStrLn $ "test10: " ++ (show test10)
