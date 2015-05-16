module Main where

import Test.HUnit
import Test.QuickCheck

import Wholemeal 


main :: IO ()
main = do
    runTestTT tests 
    fun1QuickTests
    -- fun2QuickTests -- ToDo: Doesn't run.


compareFun1 :: [Integer] -> Bool
compareFun1 xs = fun1 xs == fun1' xs

compareFun2 :: [Integer] -> Bool
compareFun2 xs = map fun2 xs == map fun2' xs

fun1QuickTests = quickCheck compareFun1 
fun2QuickTests = quickCheck compareFun2 

-- Including 2 zeroes out the product, so skip it (pretty much the trivial case)
s = [3,4,5,6,7,8]

test0 = fun1 s ~?= fun1' s
test1 = map fun2 s ~?= map fun2' s

test2 = getHeight (foldTree ['J','I'..'A']) ~?= getHeight testTree

testTree =
    Node 3
        (Node 2
            (Node 0 Leaf 'F' Leaf)
            'I'
            (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
        'J'
        (Node 2
            (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
            'H'
            (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))

test3 = xor [False, True, False] ~?= True
test4 = xor [False, True, False, False, True] ~?= False
test5 = sieveSundaram 20 ~?= [3,5,7,11,13,17,19,23,29,31,37,41] 

tests = TestList    [
                      "test0" ~: test0
                    , "test1" ~: test1
                    , "test2" ~: test2 
                    , "test3" ~: test3 
                    , "test4" ~: test4 
                    , "test5" ~: test5
                    ]
