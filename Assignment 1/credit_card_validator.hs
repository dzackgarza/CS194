import System.Environment

main = do
    print ( map validate n ) where
    n = [4012888888881881, 4012888888881882]
    -- == [True, False]
    
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = lastDigit : toDigitsRev rmDigits where
        lastDigit = mod n 10
        rmDigits = div n 10

sumDigits :: [Integer] -> Integer
sumDigits l = case l of
    x:xs -> (if x > 10 then (sumDigits( toDigits x)) else x) + (sumDigits xs)
    [] -> 0

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse( zipWith (*) (reverse xs) (cycle [1,2]))

validate :: Integer -> Bool
validate x
    | mod (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0 = True
    | otherwise = False
