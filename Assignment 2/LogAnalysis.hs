{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage (idCode:remaining) = case idCode of
	'I' -> infoHandler 		r
	'W' -> warningHandler 	r
	'E' -> errorHandler 	r
	_	-> Unknown (idCode:remaining) -- First char is not actually an id code.
	where
		r = words remaining
parseMessage _ = undefined

infoHandler :: [String] -> LogMessage
infoHandler (time:message) = LogMessage Info t m 
	where
		t = read time
		m = unwords message
infoHandler _ = undefined


warningHandler :: [String] -> LogMessage
warningHandler (time:message) =  LogMessage Warning t m 
	where
		t = read time
		m = unwords message
warningHandler _ = undefined

errorHandler :: [String] -> LogMessage
errorHandler (err_code:time:message) =  LogMessage e t m 
	where
		e = Error (read err_code)
		t = read time
		m = unwords message
errorHandler _ = undefined


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert newMsg (Node ml rootMsg mr)
	| t_new < t_root 	= Node (insert newMsg ml) rootMsg mr
	| otherwise 		= Node ml rootMsg (insert newMsg mr)
	where 
		t_new 	= getTimestamp newMsg
		t_root 	= getTimestamp rootMsg

-- Extract the timestamp from a LogMessage
getTimestamp :: LogMessage -> TimeStamp
getTimestamp (LogMessage Info 		t _) = t
getTimestamp (LogMessage Warning 	t _) = t
getTimestamp (LogMessage (Error _) 	t _) = t
getTimestamp (Unknown _) 				 = undefined


build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf 					= []
inOrder (Node l msg r)			= inOrder l ++ [msg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . filter isRelevant

isRelevant :: LogMessage -> Bool
isRelevant msg = getErrorCode msg > 50

getErrorCode :: LogMessage -> Int 
getErrorCode (LogMessage (Error i) _ _) = i
getErrorCode (LogMessage {}) = 0
getErrorCode (Unknown _) = 0

extractMessage :: LogMessage -> String
extractMessage (LogMessage (Error _) _ s) = s
extractMessage (LogMessage _ _ s) = s 
extractMessage (Unknown s) = s
