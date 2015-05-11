module Main where

import Test.HUnit

import LogAnalysis
import Log

main :: IO ()

-- runTestTT has type Test -> IO Counts, while main has type IO ()
-- So, bind to print to send it to the console.
main = runTestTT tests >>= print

-- The "~?=" operator is a shorthand for asserting equality (actual == expected)
-- See https://hackage.haskell.org/package/HUnit-1.2.2.1/docs/Test-HUnit-Base.html
-- test_i :: Test
test0 = parseMessage "E 2 562 help help" 			~?= LogMessage (Error 2) 562 "help help"
test1 = parseMessage "I 29 la la la"				~?= LogMessage Info 29 "la la la"
test2 = parseMessage "This is not the right format" ~?= Unknown "This is not the right format"

-- Inserting an unknown element into a tree should just return that tree.
test3 = insert (Unknown "") Leaf 				~?= Leaf
test4 = insert (Unknown "") (emptyInfoTree 5) 	~?= emptyInfoTree 5
test5 = insert (infoMsg 5) Leaf					~?= emptyInfoTree 5
test6 = insert (infoMsg 1) (emptyInfoTree 9) 	~?= Node (emptyInfoTree 1) 	(infoMsg 9)	Leaf
test7 = insert (infoMsg 9) (emptyInfoTree 1)	~?= Node Leaf 				(infoMsg 1)	(emptyInfoTree 9)

test8 = build [infoMsg 7, infoMsg 1, infoMsg 9, infoMsg 3] ~?= Node (Node Leaf (infoMsg 1) (emptyInfoTree 3)) (infoMsg 7) (emptyInfoTree 9)
test9 = inOrder (Node (Node Leaf (infoMsg 1) (emptyInfoTree 3)) (infoMsg 7) (emptyInfoTree 9)) ~?= [infoMsg 1, infoMsg 3, infoMsg 7, infoMsg 9]

test10 = isRelevant (LogMessage (Error 49) 0 "") ~?= False
test11 = isRelevant (LogMessage (Error 51) 0 "") ~?= True

test12 = TestCase $ do 
	actual <- testWhatWentWrong parse whatWentWrong "sample.log" 
	actual @?= expected
	where
		expected = 	[ "Way too many pickles"
					, "Bad pickle-flange interaction detected"
					, "Flange failed!"
				   	]


-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"


-- The "~:" operator is a shorthand for creating a test and attaching a label (label ~: Test)
tests = TestList 	[ 
					  "test0" 	~: test0
					, "test1" 	~: test1
					, "test2" 	~: test2
					, "test3" 	~: test3
					, "test4" 	~: test4
					, "test5" 	~: test5
					, "test6" 	~: test6
					, "test7" 	~: test7
					, "test8" 	~: test8
					, "test9" 	~: test9
					, "test10" 	~: test10
					, "test11" 	~: test11
					, "test12" 	~: test12
					]

-- Helper Functions

-- Creates a tree with on element - an "Info" Log Message with timestamp n
emptyInfoTree n = Node Leaf (infoMsg n) Leaf

-- Create an empty info message with timestamp n
infoMsg n = LogMessage Info n ""
