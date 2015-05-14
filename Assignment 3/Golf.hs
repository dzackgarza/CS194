module Golf where

import qualified Data.Map as Map
import qualified Data.List as List

-- Exercise 1: HopScotch
-- Main functions, returns a list with every n in (0, N) skipped.
skips :: [a] -> [[a]]
skips [] = [[]]
skips [x] = [[x]]
skips x = reverse $ skipsN (length x) x

-- Helper function - used to recursively build skip lists
skipsN :: Int -> [a] -> [[a]]
skipsN 0 _ = []
skipsN n x = everyN n x : skipsN (n-1) x 

-- Helper function - returns a list of every nth element from a.
everyN :: Int -> [a] -> [a]
everyN _ [] = []
everyN 0 _  = []
everyN n x 
    | n > length x    = []
    | otherwise         = last y : everyN n ys
        where
        (y, ys) = splitAt n x

-- Exercise 2: Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (y:z:xs) 
    | otherwise = localMaxima (y:z:xs)

-- Exercise 3: Histogram
histogram :: [Int] -> String 
histogram [] = footer ++ numList                            -- Base case: print the footer after the list is exhausted.
histogram xs = finalLine 
    where 
        freqMap = Map.fromList $ List.sort $ frequencies xs -- Get a set of (k,v) mapping integers to how often they occur.
        maxFreq = maximum $ Map.elems freqMap               -- Find how often the most frequent number occurred.
        maxVals = Map.filter (>= maxFreq) freqMap           -- Find the map entries that occur at least this many times
        currentLine = foldr addAsterisk emptyString (Map.keys maxVals) -- Create a string that with asterisks at these numbers.
        finalLine = currentLine ++ "\n" ++ remainingLines   -- Append this recursively to a reduced histogram
            where                                           -- For each value just printed, remove one occurrence from the original list and recurse.
                remainingLines  = histogram (removeItems xs (Map.keys maxVals)) 

-- Remove a list of items (d) from another list (l).
removeItems :: [Int] -> [Int] -> [Int]
removeItems l [] = l
removeItems [] _= []
removeItems l d = removeItems (List.delete d' l) (List.delete d' d)
    where d' = head d

-- Take a position and a base string, and replace that character with an asterisk.
addAsterisk :: Int -> String -> String
addAsterisk n s = prefix ++ "*" ++ postfix
    where
        prefix = map (s !!) [0..n-1]
        postfix = map (s !!) [n+1..9]


-- Count frequencies of elements in list
frequencies :: [Int] -> [(Int, Int)]
frequencies lst = map (\x -> (head x, List.genericLength x)) 
    $ List.group $ List.sort lst

-- Base string for folding operations - insert asterisks wherever necessary.
emptyString :: String
emptyString = "          \n"

-- The numbers in the footer.
numList :: String
numList = "0123456789\n"

-- The separator in the footer.
footer :: String
footer = "==========\n"
