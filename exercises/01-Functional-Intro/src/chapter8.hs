{-
  Name: Bryan Castillo
  Exercises: Chapter 8 (Declaring types and classes) "Programming in Haskell"
-}
import Data.List (all)
import Data.Char

all_true xs = all id xs

{-
  1. In a similar manner to the function add, define a recursive multiplication
     function mult :: Nat -> Nat -> Nat for the recursive type of natural
     numbers:

     Hint: make use of add in your definition.
-}

-- From book

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- multiplication

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = n `add` (m `mult` n)

test1 =
  all_true [
    (nat2int $ (int2nat 5) `mult` (int2nat 6)) == 30,
    (nat2int $ (int2nat 1) `mult` (int2nat 6)) == 6,
    (nat2int $ (int2nat 7) `mult` (int2nat 1)) == 7,
    (nat2int $ Zero `mult` (int2nat 6)) == 0,
    (nat2int $ (int2nat 7) `mult` Zero) == 0
  ]

{-
  2. Although not included in appendix B, the standard prelude defines

      data Ordering = LT | EQ | GT

     together with a function compare :: Ord a => a -> a -> Ordering

     that decides if one value in an ordered type is less than (LT), equal to
     (EQ), or greater than (GT) another value. Using this function, redefine
     the function occurs :: Ord a => a -> Tree a -> Bool for search trees.
     Why is this new definition more efficient than the original version?
-}

-- from book

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs x (Leaf y) = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r

occursOrd :: Ord a => a -> Tree a -> Bool
occursOrd x (Leaf y) = x == y
occursOrd x (Node left y right) =
  case (compare x y) of
    EQ -> True
    LT -> occursOrd x left
    GT -> occursOrd x right

test2 =
  let my_tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
  in
    all_true (
      (map (\x -> occursOrd x my_tree) [1,3,4,5,6,7,9]) ++
      (map (\x -> not $ occursOrd x my_tree) [0,2,8,10])
    )

{-
  3. Consider the following type of binary trees:

      data Tree a = Leaf a | Node (Tree a) (Tree a)

     Let us say that such a tree is balanced if the number of leaves in the
     left and right subtree of every node differs by at most one, with
     leaves themselves being trivially balanced. Define a function
       balanced :: Tree a -> Bool
     that decides if a binary tree is balanced or not.

     Hint: first define a function that returns the number of leaves in a tree.
-}

data BTree a = BLeaf a | BNode (BTree a) (BTree a)

-- This version is a little faster than sBalanced
balanced :: BTree a -> Bool
balanced t =
  let
    ibalanced :: BTree a -> (Bool, Int)
    ibalanced (BLeaf _) = (True, 1)
    ibalanced (BNode left right) =
      let
        (lb, lc) = ibalanced left
        (rb, rc) = ibalanced right
      in (lb && rb && abs(lc - rc) <= 1, lc + rc)
  in fst (ibalanced t)

-- This is the version based on the hint from the book. Its a bit slower.
leafs :: BTree a -> Int
leafs (BLeaf _) = 1
leafs (BNode left right) = (leafs left) + (leafs right)

sBalanced :: BTree a -> Bool
sBalanced (BLeaf _) = True
sBalanced (BNode left right) =
  (balanced left) && (balanced right) && (abs ((leafs left) - (leafs right))) <= 1

makeBTree :: Int -> BTree Int
makeBTree 1 = BLeaf 1
makeBTree n
  | n > 1 = BNode (makeBTree (n - 1)) (makeBTree (n - 1))

-- Test it

test3 =
  let
    bigTree = makeBTree 20
  in
    all_true [
      balanced $ BLeaf 1,
      balanced $ BNode (BLeaf 1) (BLeaf 2),
      balanced $ BNode (BNode (BLeaf 1) (BLeaf 1)) (BLeaf 2),
      balanced $ BNode (BLeaf 0) (BNode (BLeaf 1) (BLeaf 1)),
      not $ balanced $ BNode (BLeaf 0) (BNode (BLeaf 1) (BNode (BLeaf 3) (BLeaf 4))),
      not $ balanced $ BNode (BNode (BLeaf 1) (BNode (BLeaf 3) (BLeaf 4))) (BLeaf 0),
      balanced bigTree
    ]

{-
  4. Define a function balance :: [a] -> Tree a that converts a non-empty list
     into a balanced tree.

     Hint: first define a function that splits a list into two halves whose
           length differs by at most one.
-}

splitList :: [a] -> ([a], [a])
splitList xs =
  let
    takeSkip [] = []
    takeSkip (x:xs) = x : skipTake xs
    skipTake [] = []
    skipTake (_:xs) = takeSkip xs
  in (takeSkip xs, skipTake xs)

makeBalancedTree :: [a] -> BTree a
makeBalancedTree [] = error "empty list!"
makeBalancedTree [x] = BLeaf x
makeBalancedTree xs =
  let
    (l1,l2) = splitList xs
    t1 = makeBalancedTree l1
    t2 = makeBalancedTree l2
  in BNode t1 t2

test4 =
  all_true [balanced $ makeBalancedTree [1..x] | x <- [1..100]]

{-
  5. Given the type declaration

       data Expr = Val Int | Add Expr Expr

     define a higher-order function

       folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

     such that folde f g replaces each Val constructor in an expression by
     function f, and each Add constructor by the function g.
-}

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add le re) = g (folde f g le) (folde f g re)

{-
  6. Using folde, define a function eval :: Expr -> Int that evaluates an
     expression to an integer value, and a function size :: Expr -> Int
     that calculates the number of values in an expression.
-}

exprEval :: Expr -> Int
exprEval = folde id (+)

exprSize :: Expr -> Int
exprSize (Val _) = 1
exprSize (Add le re) = exprSize le + exprSize re

test6 =
  all_true [
    exprEval (Val 5) == 5,
    exprEval (Add (Val 5) (Val 7)) == 12,
    exprEval (Add (Val 5) (Add (Val 1) (Val 2))) == 8,
    exprSize (Val 5) == 1,
    exprSize (Add (Val 5) (Val 7)) == 2,
    exprSize (Add (Val 5) (Add (Val 1) (Val 2))) == 3
  ]

{-
  7. Complete the following instance declarations:

      instance Eq a => Eq (Maybe a) where


      instance Eq a => Eq [a] where
-}

data MyMaybe a = MyNothing | MyJust a

instance Eq a => Eq (MyMaybe a) where
  MyNothing == MyNothing = True
  (MyJust a) == (MyJust b) = a == b
  _ == _ = False

test7 =
  all_true [
    (MyNothing :: MyMaybe Int) == (MyNothing :: MyMaybe Int),
    (MyNothing :: MyMaybe Int) /= (MyJust 8),
    (MyJust 8) /= (MyNothing :: MyMaybe Int),
    (MyJust 8) == (MyJust 8),
    (MyJust 7) /= (MyJust 6)
  ]

{-
  8. Extend the tautology checker to support the use of logical disjuntion (v)
     and equivalence (<=>) in propositions.
-}

type Assoc k v = [(k,v)]

find :: Eq k => k -> (Assoc k v) -> v
find k pairs = head [v' | (k',v') <- pairs, k' == k]

data Prop =
  Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Equal Prop Prop
  | Or Prop Prop
  deriving (Show)

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equal p q) = eval s p == eval s q
eval s (Or p q)    = eval s p || eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = [ v : vs | v <- [True,False], vs <- bools (n - 1)]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 =
  Imply
    (And
      (Var 'A')
      (Imply (Var 'A') (Var 'B')))
    (Var 'B')

p5 = (Or (Var 'A') (Not $ Var 'A'))
p6 = (Equal (Var 'A') (Var 'A'))

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

test8 =
  all_true [
    not $ isTaut p1,
    isTaut p2,
    not $ isTaut p3,
    isTaut p4,
    isTaut p5,
    isTaut p6
  ]

{-
  9. Extend the abstract machine to support the use of multiplication.
-}

data MExpr = MVal Int | MAdd MExpr MExpr

mValue :: MExpr -> Int
mValue (MVal n) = n
mValue (MAdd x y) = (mValue x) + (mValue y)


main :: IO ()
main = do
  putStrLn "Running chapter 8 tests:"
  putStrLn $ "test1: " ++ (show test1)
  putStrLn $ "test2: " ++ (show test2)
  putStrLn $ "test3: " ++ (show test3)
  putStrLn $ "test4: " ++ (show test4)
  putStrLn $ "test6: " ++ (show test6)
  putStrLn $ "test7: " ++ (show test7)
  putStrLn $ "test8: " ++ (show test8)
