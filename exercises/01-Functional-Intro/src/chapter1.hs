{-
  Name: Bryan Castillo
  Exercises: Chapter 1 "Programming in Haskell"
-}

{-
   1. Give another possible calculation for the result of:
        double (double 2)

   Answer:

     Definition: double x = x + x

     double (double 2)
     { apply outer double }
     (double 2) + (double 2)
     { applying first double }
     2 + 2 + (double 2)
     { applying + }
     4 + (double 2)
     { applying double }
     4 + 2 + 2
     { applying + }
     8

     double (double 2)
     { apply inner double }
     double (2 + 2)
     { apply double }
     (2 + 2) + (2 + 2)
     { apply + }
     8
-}

{-
    2. Show that sum[x] = x for any number x.

    Answer:

    Definition:
      sum [] = 0
      sum (n:ns) = n + sum ns

    sum [x]
    { apply sum }
    x + sum []
    { apply sum }
    x + 0
    { apply + }
    x
-}

{-
    3. Define a function product that produces the product of a list of
       numbers, and show using your definition that [2,3,4] = 24
-}

-- note: product is already in Prelude.
myProduct [] = 1
myProduct (n:ns) = n * myProduct ns

-- Test the product function
test3 = myProduct [2,3,4] == 24

{-
    4. How should the definition of the function qsort be modified so that
       it produces a reversed sorted version of a list?

    Definition of qsort from book:

    qsort [] = []
    qsort (x:xs) =
      qsort smaller ++ [x] +++ qsort larger
      where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

    Answer: Swap smaller for larger.
-}
rqsort [] = []
rqsort (x:xs) =
  rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

test4 = (rqsort [1,2,7,5]) == [7,5,2,1]

{-
    5. What would be the effect of replacing <= by < in the original
        definition of qsort? Hint: consider the example qsort [2,2,3,1,1]

    Answer:
      It removes duplicate values.
-}

badqsort [] = []
badqsort (x:xs) =
  badqsort smaller ++ [x] ++ badqsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger  = [b | b <- xs, b > x]

test5 = (badqsort [2,2,3,1,1]) == [1,2,3]

main = do
  putStrLn "Running chapter1 tests:"
  putStrLn $ "Test3: " ++ (show test3)
  putStrLn $ "Test4: " ++ (show test4)
  putStrLn $ "Test5: " ++ (show test5)
