module Main where

import Test.HUnit

import Golf

main :: IO ()

main = runTestTT tests >>= print

test0 = skips "ABCD"           ~?= ["ABCD", "BD", "C", "D"]
test1 = skips "hello!"         ~?= ["hello!", "el!", "l!", "l", "o", "!"]
test2 = skips ([1] :: [Int])   ~?= ([[1]] :: [[Int]])
test3 = skips [True,False]     ~?= [[True,False], [False]]
test4 = skips ([] :: [String]) ~?= ([[]] :: [[String]])

test5 = everyN 2 ([1,2,3,4,5,6,7,8,9] :: [Int]) ~?= ([2,4,6,8] :: [Int])

test6 = localMaxima [2,9,5,6,1] ~?= [9,6]
test7 = localMaxima [2,3,4,1,5] ~?= [4]
test8 = localMaxima [1,2,3,4,5] ~?= []

test9 = histogram [3,5] ~?= "   * *    \n==========\n0123456789\n"

tests = TestList    [
                      "test0" ~: test0
                    , "test1" ~: test1
                    , "test2" ~: test2
                    , "test3" ~: test3
                    , "test4" ~: test4
                    , "test5" ~: test5
                    , "test6" ~: test6
                    , "test7" ~: test7
                    , "test8" ~: test8
                    , "test9" ~: test9
                    ]
