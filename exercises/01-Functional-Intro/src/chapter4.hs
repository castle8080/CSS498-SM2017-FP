{-
  Name: Bryan Castillo
  Exercises: Chapter 4 (Defining functions) "Programming in Haskell"
-}
import Data.List
import Debug.Trace

-- Utility function for checking all items in a list are true.
all_true xs = all id xs

{-
  1. Using library functions, define a function:
      halve :: [a] -> ([a],[a])
     that splits an even-lengthed list into two halves.
     For example:
       > halve [1,2,3,4,5,6]
       ([1,2,3],[4,5,6])
-}

halve :: [a] -> ([a],[a])
halve xs =
  ((take ((length xs) `div` 2) xs), (drop ((length xs) `div` 2) xs))

test1 =
  all_true [
    (halve [1..6]) == ([1,2,3], [4,5,6]),
    (halve [1..5]) == ([1,2], [3,4,5]),
    (halve [1]) == ([], [1])
  ]

{-
  2. Define a function
       third :: [a] -> a
     that returns the third element in a list that contains at least
     this many elements using:

     a. head and tail;
     b. list indexing !!;
     c. pattern matching;
-}

third_a :: [a] -> a
third_a xs = head $ tail $ tail $ xs

third_b :: [a] -> a
third_b xs = xs !! 2

third_c :: [a] -> a
third_c (xa:xb:xc:xs) = xc

-- Testing
check_third xs expected =
  all_true [
    (third_a xs) == expected,
    (third_b xs) == expected,
    (third_c xs) == expected
  ]

test2 =
  all_true [
    check_third [1..6] 3,
    check_third [1,7,8] 8
  ]

{-
  3. Consider a function
       safetail :: [a] -> [a]
     that behaves in the same way as tail except that it maps the
     empty list to itself rather than producing an error.
     Using tail and the function
       null :: [a] -> Bool
     that decide if a list is empt yor not, define safetail using:

       a. a conditional expression;
       b. guarded equations;
       c. pattern matching.
-}

safetail_a :: [a] -> [a]
safetail_a xs =
  if (null xs) then
    []
  else
    tail xs

safetail_b :: [a] -> [a]
safetail_b xs
  | null xs = []
  | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c [] = []
safetail_c xs = tail xs

-- testing
check_tail :: Eq a => [a] -> [a] -> Bool
check_tail xs expected =
  all_true [
    (safetail_a xs) == expected,
    (safetail_b xs) == expected,
    (safetail_c xs) == expected
  ]

test3 =
  all_true [
    check_tail ([] :: [Int]) ([] :: [Int]),
    check_tail [1] [],
    check_tail [1,2] [2],
    check_tail [1..6] [2..6]
  ]

{-
  4. In a similar way to && in section 4.4, show how the
      disjunction operator || can be defined in four different ways
      using pattern matching.
-}

or_a :: Bool -> Bool -> Bool
True  `or_a` True  = True
True  `or_a` False = True
False `or_a` True  = True
False `or_a` False = False

or_b :: Bool -> Bool -> Bool
False `or_b` False = False
_ `or_b` _ = True

or_c :: Bool -> Bool -> Bool
True `or_c` _ = True
False `or_c` b = b

or_d :: Bool -> Bool -> Bool
_ `or_d` True = True
b `or_d` False = b

-- testing
check_or b1 b2 expected =
  all_true (map (\f -> b1 `f` b2 == expected) [or_a, or_b, or_c, or_d])

test4 =
  all_true [
    check_or True True True,
    check_or True False True,
    check_or False True True,
    check_or False False False
  ]

-- The short circuit test shows lazy evaluation.
-- You can see for or_b and or_c that the 2nd boolean is
-- never evaluated.
my_bool v =
  trace ("Returning: " ++ (show v)) v

test4_shortcircuit = do
  putStrLn $ "Running short circuit test4:"
  putStrLn $ "or_a: " ++ (show (True `or_a` (my_bool True)))
  putStrLn $ "or_b: " ++ (show (True `or_b` (my_bool True)))
  putStrLn $ "or_c: " ++ (show (True `or_c` (my_bool True)))
  putStrLn $ "or_d: " ++ (show (True `or_d` (my_bool True)))

{-
  5. Without using any other library functions or operators, show how
     the meaning of the following pattern matching definition for
     logical conjunction && can be formalised using
     conditional expressions:

       True && True = True
       _    && _    = False

    Hint: use two nested conditional expressions.
-}

a `and_5` b =
  if a then
    if b then True else False
  else
    False

test5 =
  all_true [
    True `and_5` True,
    True `and_5` False == False,
    False `and_5` True == False,
    False `and_5` False == False
  ]

{-
  6. Do the same for the following alternative definition, and note the
     difference in the number of conditional expressions that are required:
       True && b = b
       False && _ = False
-}

a `and_6` b = if a then b else False

test6 =
  all_true [
    True `and_6` True,
    True `and_6` False == False,
    False `and_6` True == False,
    False `and_6` False == False
  ]

{-
  7. Show how the meaning of the following curried function definition can
     be formalised in terms of lambda expressions:

     mult :: Int -> Int -> Int -> Int
     mult x y z = x * y * z
-}
mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x * y * z)))

test7 = (mult 7 3 5) == 7 * 3 * 5

{-
  8. The Luhn algorithm is used to check bank card numbers for simple errors
     such as mistyping a digit, and proceeds as follows:
       * consider each digit as a separate number;
       * moving left, double every other number from the second last;
       * substract 9 from each number that is now creater than 9;
       * add all the resulting numbers together;
       * if the total is divisble by 10, the card number is valid.

    Define a function luhnDouble :: Int -> Int that doubles a digit and
    subtracts 9 if the result is greater than 9.

    For example:

    > luhnDouble 3
    6

    > luhnDouble 6
    3

    Using luhnDouble and the integer remainder function mod, define a function
      luhn :: Int -> Int -> Int -> Int -> Bool
    that decides if a four-digit bank card number is valid. For example:

    > luhn 1 7 8 4
    True

    > lyhn 4 7 8 3
    False
-}
luhnDouble :: Int -> Int
luhnDouble x =
  if (x * 2) > 9 then x * 2 - 9
  else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
  ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0

test8 =
  all_true [
    luhn 1 7 8 4,
    not $ luhn 4 7 8 3
  ]

main = do
  putStrLn $ "Running chapter 4 tests:"
  putStrLn $ "test1: " ++ (show test1)
  putStrLn $ "test2: " ++ (show test2)
  putStrLn $ "test3: " ++ (show test3)
  putStrLn $ "test4: " ++ (show test4)
  test4_shortcircuit
  putStrLn $ "test5: " ++ (show test5)
  putStrLn $ "test6: " ++ (show test6)
  putStrLn $ "test7: " ++ (show test7)
  putStrLn $ "test8: " ++ (show test8)
