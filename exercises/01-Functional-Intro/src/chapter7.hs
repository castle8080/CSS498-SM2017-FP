{-
  Name: Bryan Castillo
  Exercises: Chapter 7 (Higher-order functions) "Programming in Haskell"
-}
import Data.List (all)
all_true xs = all id xs

{-
  1. Show how the list comprehension [f x | x <- xs, p x] can be
     re-expressed using the higher-order functions map and filter.
-}

mfilter :: (a -> Bool) -> (a -> b) -> [a] -> [b]
mfilter f p = map p . filter f

test1 =
  mfilter (> 5) (* 2) [0..10] == [x * 2 | x <- [0..10], x > 5]

{-
  2. Without looking at the definitions from the standard prelude, define the
  following higher-order library functions on lists.

    a. Decide if all elements of a list satisfy a predicate.
        all :: (a -> Bool) -> [Bool] -> Bool

    b. Decide if any element of a list satisfies a predicate:
        any :: (a -> Bool) -> [Bool] -> Bool

    c. Select elements from a list while they satisfy a predicate:
       takeWhile :: (a -> Bool) -> [a] -> [a]

    d. Remove elements from a list while they satisfy a predicate:
       dropWhile :: (a -> Bool) -> [a] -> [a]

    Note: in the prelude the first two of these functions are generic functions
    rather than being specific to the type of lists.
-}

my_all :: (a -> Bool) -> [a] -> Bool
my_all p =  and . map p

my_any :: (a -> Bool) -> [a] -> Bool
my_any p = or . map p

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile p (x:xs)
  | p x = x : my_takeWhile p xs
my_takeWhile _ _ = []

my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile p (x:xs)
  | p x =  my_dropWhile p xs
my_dropWhile _ xs = xs

test2 =
  all_true [
    my_all (>= 5) [5..10],
    not $ my_all (>= 5) [5,1,10],
    my_all (>= 5) [],
    not $ my_any (< 10) [],
    my_any (< 10) [9],
    my_any (< 10) [11,1],
    not $ my_any (< 10) [10],
    my_takeWhile (< 5) [1..10] == [1..4],
    my_takeWhile (< 5) [1..4] == [1..4],
    my_takeWhile (<5) [10..100] == [],
    my_dropWhile (<5) [1..10] == [5..10]
  ]

{-
  3. Redefine the functions map f and filter p using foldr.
-}

my_map :: (a -> b) -> [a] -> [b]
my_map f = foldr (\x ys -> (f x) : ys) []

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter p = foldr (\x xs -> if p x then x : xs else xs) []

test3 =
  all_true [
    my_map (*2) [0..3] == [0,2,4,6],
    my_filter (<=2) [0..10] == [0,1,2]
  ]

{-
  4. Using foldl, define a function dec2int :: [Int] -> Int that converts a
     decimal number into an integer. For example:
       > dec2int [2,3,4,5]
       2345
-}

dec2int :: [Int] -> Int
dec2int = foldl (\total d -> total * 10 + d) 0

test4 =
  all_true [
    dec2int [2,3,4,5] == 2345,
    dec2int [0,0,1] == 1,
    dec2int [0] == 0,
    dec2int [0,9,0,0] == 900
  ]

{-
  5. Without looking at the definitions from the standard prelude, define
     the higher-order library function curry that converts a function on
     pairs into a curried function, and, conversely, the function uncurry
     that converts a curried function with two arguments into a function on
     pairs.

     Hint: first write down the types of the two functions.
-}

my_curry :: ((a,b) -> c) -> (a -> b -> c)
my_curry f = \a b -> f (a, b)

my_uncurry :: (a -> b -> c) -> ((a,b) -> c)
my_uncurry f = \(a,b) -> f a b

test5 =
  all_true [
    (my_curry (\(x,y) -> x + y)) 4 5 == 9,
    (my_uncurry (+)) (6,7) == 13
  ]

{-
  6. A higher-order function unfold that encapsulates a simple pattern of
     recursion for producing a list can be defined as follows:

       unfold p h t x | p x = []
                      | otherwise = h x : unfold p h t ( t x)

     That is, the function unfold p h t produces the empty list if the predicate
     p is true of the argument value, and otherwise produces a non-empty list by
     applying the funciton h to this value to give the head, and the function t
     to generate another argument that is recursively processed in the same way
     to produce the tail of the list. For example, the function int2bin can be
     rewritten more compactly using unfold as follows:

       int2bin = unfold (== 0) (`mod` 2) (`div` 2)

     Redefine the functions chop8, map f and iterate f using unfold.
-}

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t ( t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

my_map2 :: (a -> b) -> [a] -> [b]
my_map2 f = unfold null (f . head) tail

my_iterate :: (a -> a) -> a -> [a]
my_iterate f = unfold (const False) f f

test6 =
  all_true [
    chop8 [1..16] == [[1..8], [9..16]],
    my_map2 (*2) [0..3] == [0,2,4,6],
    (take 4 $ my_iterate (*2) 1) == [2,4,8,16]
  ]

main :: IO ()
main = do
  putStrLn "Running chapter 7 tests:"
  putStrLn $ "test1: " ++ (show test1)
  putStrLn $ "test2: " ++ (show test2)
  putStrLn $ "test3: " ++ (show test3)
  putStrLn $ "test4: " ++ (show test4)
  putStrLn $ "test5: " ++ (show test5)
  putStrLn $ "test6: " ++ (show test6)
