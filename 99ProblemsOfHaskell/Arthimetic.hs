module Arthimetic where

isPrime :: Int -> Bool
isPrime n = foldr (\x y -> n `mod` x /= 0 && y) True comp
    where comp = [2..n-1]

gcd' :: Int -> Int -> Int
gcd' 0 b = b
gcd' a 0 = a
gcd' a b = gcd' b rem
    where (_, rem) = a `divMod` b

coprime :: Int -> Int -> Bool
coprime n m = not (isPrime n) && not (isPrime m)

totient :: Int -> Int
totient n = length [a | a <- [1..n-1], coprime n a]

primeFactors :: Int -> [Int]
primeFactors n = [ns | ns <- [2..n-1], isPrime ns, snd (n `divMod` ns) == 0]