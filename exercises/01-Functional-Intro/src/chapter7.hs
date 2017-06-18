{-
  Name: Bryan Castillo
  Exercises: Chapter 7 (Higher-order functions) "Programming in Haskell"
-}
import Data.List (all)
import Data.Char
import Control.Exception

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

{-
  7. Modify the binary string transmitter example to detect simple transmission
     errors using the concept of parity bits. That is, each eight-bit binary
     number produced during encoding is extended with a parity bit, set to one
     if the number contains an odd number of ones, and to zero otherwise. In
     turn, each resulting nine-bit binary number consumed during decoding is
     checked to ensure that its parity bit is correct, with the parity Bit
     being discarded if this is the case, and a parity error being
     reported otherwise.

     Hint: the library function error :: String -> a displays the given string
     as an error message and terminates the program; the polymorphic result
     type ensures that error can be used in any context.

     Answer:

     The code below was modified from the book.

     The sample ghci session below shows a parity bit that is invalid.

      *Main> let x = encode "hi"
      *Main> x
      [0,0,0,1,0,1,1,0,1,1,0,0,1,0,1,1,0,0]
      *Main> x !! 8
      1
      *Main> decode $ take 8 x ++ [0] ++ drop 9 x
      "*** Exception: Invalid parity bit
-}

chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold null (take n) (drop n)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bits = if sum bits `mod` 2 == 1 then 1 else 0

addParity :: [Bit] -> [Bit]
addParity bits = bits ++ [parity bits]

decodePacket :: [Bit] -> [Bit]
decodePacket bits =
  let
    dataBits = take 8 bits
    in
      if bits !! 8 /= parity dataBits then
        error "Invalid parity bit"
      else
        dataBits

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int . decodePacket) . (chop 9)

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

test7 =
  all_true [
    transmit "is this easy" == "is this easy"
  ]

{-
  8. Test your new string transmitter program from the previous exercise
     using a faulty communication channel that forgets the first bit, which
     can be modelled using the tail function on lists of bits.
-}

badChannel :: [Bit] -> [Bit]
badChannel = tail

badTransmit :: String -> String
badTransmit = decode . badChannel . encode

-- I can not figure out how to catch error in Haskell yet.
-- *Main> badTransmit "hi"
-- "*** Exception: Invalid parity bit

{-
  9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that
    alternately applits its two argument functions to successive elements
    in a list, in turn about order. For example:

    > altMap (+10) (+100) [0,1,2,3,4]
    [10,101,12,103,14]
-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 = zipWith ($) (concat $ repeat [f1,f2])

test9 =
  all_true [
    altMap (+10) (+100) [0,1,2,3,4] == [10,101,12,103,14]
  ]

{-
  10. Using altMap, define a function luhn :: [Int] -> Bool that implements
      the Luhn algorithm from the exercises in chapter 4 for bank card numbers
      of any length. Test your new function using your own bank card.
-}

luhn digits =
  isDivisible $ sum $ altMap id luhnDouble (reverse digits)
    where
      sub9 n = if n > 9 then n - 9 else n
      luhnDouble = sub9 . (*2)
      isDivisible n = n `mod` 10 == 0

test10 =
  all_true [
    luhn [1,7,8,4],
    not $ luhn [1,7,8,3]
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
  putStrLn $ "test7: " ++ (show test7)
  putStrLn $ "test9: " ++ (show test9)
  putStrLn $ "test10: " ++ (show test10)
