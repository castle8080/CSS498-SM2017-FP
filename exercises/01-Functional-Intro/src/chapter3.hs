{-
  Name: Bryan Castillo
  Exercises: Chapter 3 (Types and classes) "Programming in Haskell"
-}
import Data.List

{-
  1. What are the types of the following values?
    ['a','b','c']
    ('a','b','c')
    [(False,'0'),(True,'1')]
    ([False,True],['0','1'])
    [tail, init, reverse]

  Answer:
    The following definitions specify the types matching the values above.
    (This seems similar to prototypes in C.)
-}

one_a :: [Char]
one_a = ['a','b','c']

one_b :: (Char, Char, Char)
one_b = ('a','b','c')

one_c :: [(Bool,Char)]
one_c = [(False,'0'),(True,'1')]

one_d :: ([Bool],[Char])
one_d = ([False,True],['0','1'])

one_e :: [[a] -> [a]]
one_e = [tail, init, reverse]

{-
  2. Write down definitions that have the following types; it does not matter
     what the definitions actually do as long as they are type correct.
    bools :: [Bool]
    nums :: [[Int]]
    add :: Int -> Int -> Int
    copy :: a -> (a,a)
    apply :: (a -> b) -> a -> b
-}

-- Answer:

bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[1]]

add :: Int -> Int -> Int
add x y = x + y

copy :: a -> (a,a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f arg = (f arg)

-- Test the functions
test2 =
  all id [
    add 1 2 == 3,
    copy "bob" == ("bob", "bob"),
    copy 42 == (42, 42),
    apply copy 7 == (7, 7)
  ]

{-
  3. What are the types of the following functions?

    second xs = head (tail xs)
    swap (x,y) = (y,x)
    pair x y = (x,y)
    double x = x * 2
    palindrome xs = reverse xs == xs
    twice f x = f (f x)

    Answer: types listed below and checked.
-}

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-
  5. Why is it not feasible in general for function types to be instances of
     the Eq class? When is it feasible? Hint: two functions of the same type
     are equal if they always return equal results for equal arguments.

  Answer:

    It would be difficult to determine if 2 functions produce the same values
    for the same arguments. There are 2 ways I can think of to check
    equality of functions:

    1. Test all possible arguments of a given type against both functions.
       This would be impossible for types that have an infinite number
       of values.
    2. The only other way I can think of is to look at the structure
       of both functions and see if they reduce to the same code.
       Based on the first chapter showing how to reason about functions
       using substitution, I would think this could be done if the
       code structure could be checked programatically. I don't think
       that Haskell has the ability to examine the structure of arbitrary
       functions though.
-}

main = do
  putStrLn "Running chapter3 tests:"
  putStrLn $ "Test2: " ++ (show test2)
