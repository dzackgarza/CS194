module Wholemeal where

import Data.List
-- Exercise 1: Wholemeal programming'

-- Original functions:
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x-2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1) 

-- Idiomatic functions
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even  


fun2' :: Integer -> Integer
fun2' x = sum (filter even $ tail $ takeWhile (/=1) $ iterate f x)
        + (if even x then x else 0) -- Shim to include first element if it's odd.

-- Performs the "Collatz Conjecture" process in fun2'.
f n = if even n then n `div` 2 else 3*n+1 

-- Exercise 2: Folding With Trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree [] = Leaf
foldTree [a] = Node 0 Leaf a Leaf
foldTree xs = foldr treeInsert Leaf xs

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ l val r)
    | getHeight l >= getHeight r  =  Node (newRightHeight+1) l val newRightTree
    | otherwise = Node (newLeftHeight+1) newLeftTree val r 
        where
            newRightTree   = treeInsert x r
            newRightHeight = getHeight newRightTree
            newLeftTree    = treeInsert x l
            newLeftHeight  = getHeight newLeftTree

getHeight :: Ord a => Tree a -> Integer
getHeight Leaf = -1         -- Adjusted by -1 to align heights with test cases.
getHeight (Node h _ _ _) = h

-- Exercise 3: More Folds

-- 1: Implement xor over lists with a fold. Returns True iff there are an odd number of Trues.
xor :: [Bool] -> Bool
xor = foldr opXor False 

-- Truth table for XOR
opXor :: Bool -> Bool -> Bool
opXor True  False   = True
opXor False True    = True
opXor True  True    = False
opXor False False   = False

-- 2: Implement map with folds
map' :: (a -> b) -> [a] -> [b]
map' g = foldr func [] 
    where
        func a b = g a : b

-- Exercise 4: Finding Primes.

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\l -> (2*l)+1) ([1..n] \\ np)
    where
        np = filter (<=n)  [i + j + 2*i*j | i <- [1..n], j <- [1..n], i <= j] 

