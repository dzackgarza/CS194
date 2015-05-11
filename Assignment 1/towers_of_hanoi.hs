import System.Environment

main = do
    print ( hanoi 2 "a" "b" "c" )
    
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = case n of
    0 -> []
    otherwise -> hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

