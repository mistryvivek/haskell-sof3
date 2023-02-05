import Prelude

{- Any number that is even is added to one.-}
oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  n `mod` 2 == 0 ]
oneone' [] = []
oneone' ns = if (head ns) `mod` 2 == 0
             then [head ns + 1] ++ oneone' rest
             else [] ++ oneone' rest
   where rest = tail ns

{- Mod index value by 2 and returns if true for all lower case letters-}
onetwo, onetwo' :: [String] -> [Bool]
onetwo css = [(fromEnum c) `mod` 2 == 1 |
              cs <- css, length cs > 1, c <- cs, c `elem` ['a'..'z']]

onetwoassist :: String -> [Bool]
onetwoassist [] = []
onetwoassist str@(s:tr) = if s `elem` ['a'..'z']
                          then [(fromEnum s) `mod` 2 == 1] ++ onetwoassist tr
                          else onetwoassist tr

onetwo' [] = []
onetwo' css@(c:ss) = onetwoassist c ++ onetwo' ss

{-Takes a boolean list and converts to boolean.
Starts on the left, takes in a lamba function with parameters n and b which does 2*n + 1(if B is true).

Parity takes a boolean list and returns a boolean. Parity returns true if there is a even number of trues in
the list.

The main function applies the function bitstring2int to every filtered subset according to parity.-}
bitstring2int :: [Bool] -> Int
bitstring2int = foldl (\ n b -> 2*n + if b then 1 else 0) 0

parity :: [Bool] -> Bool
parity = (==0) . (`mod` 2) . length . filter (== True) 

onethree ,onethree' :: [[Bool]] -> [Int]
onethree = map bitstring2int . filter parity
onethree' css = [(bitstring2int cs)|
          cs <- css, parity cs == True]

data Maybe a = Nothing | Just a

ePbs2i :: [Bool] -> Int
ePbs2i bs | parity bs = bitstring2int bs
{- Anything false according to parity is not returned.-}

{-Q2.2: Stuck-}

{-Q3.1:

(if True then inc else dbl) (2+1)
Inc (2+1)
Inc 3-}

ones, nats :: [Integer]
ones = 1 : ones
nats = 0 : map succ nats

{-Base case:

One: 1
Nats: 0

Recursive Case:

Ones: ": ones"
Nates: ": map succ nats"-}

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

