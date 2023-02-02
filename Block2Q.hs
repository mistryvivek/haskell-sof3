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

Parity takes a boolean list and returns a boolean.

The main function applies the function bitstring2int to every filtered subset according to parity.-}
bitstring2int :: [Bool] -> Int
bitstring2int = foldl (\ n b -> 2*n + if b then 1 else 0) 0

parity :: [Bool] -> Bool
parity = (==0) . (`mod` 2) . length . filter (== True) 

onethree, onethree' :: [[Bool]] -> [Int]
onethree = map bitstring2int . filter parity
onethree' = undefined

data Maybe a = Nothing | Just a





