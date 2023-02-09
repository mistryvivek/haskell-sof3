{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Prelude
import Control.Exception (bracketOnError)

{- Any number that is even is added to one.-}
oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  even n]
oneone' [] = []
oneone' ns = if even (head ns)
             then (head ns + 1) : oneone' rest
             else oneone' rest
   where rest = tail ns

{- Mod index value by 2 and returns if true for all lower case letters-}
onetwo, onetwo' :: [String] -> [Bool]
onetwo css = [fromEnum c `mod` 2 == 1 |
              cs <- css, length cs > 1, c <- cs, c `elem` ['a'..'z']]

onetwoassist :: String -> [Bool]
onetwoassist [] = []
onetwoassist str@(s:tr) = if s `elem` ['a'..'z']
                          then (fromEnum s `mod` 2 == 1) : onetwoassist tr
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
onethree' css = [bitstring2int cs|
          cs <- css, parity cs]

ePbs2i :: [Bool] -> Int
ePbs2i bs | parity bs = bitstring2int bs
{- Anything false according to parity is not returned.-}

{-Q2.2-}

ePbs2iM :: Maybe [Bool] -> Maybe Int
ePbs2iM (Just bs) | parity bs = Just (bitstring2int bs)
                  | otherwise = Nothing
ePbs2iM Nothing = Nothing

doubleOdd :: Int -> Int
doubleOdd n | odd n = n * 2

doubleOddM :: Maybe Int -> Maybe Int
doubleOddM Nothing = Nothing
doubleOddM (Just o) | o `mod` 2 == 1 = Just (o * 2)
                    | otherwise = Nothing

{- 
doepM :: [Bool] -> String
doepM a = maybe "Oops!" (bitstring2int a * 2)


doepE :: [Bool] -> String
doepE = undefined

type Error a = Either String a

doubleOddE :: Error Int -> Error Int
doubleOddE  (Left msg) = 
doubleOddE  (Right o) | o `mod` 2 == 1 = Right (o * 2)
                       | otherwise = Left "Must be an odd number!"
-}

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

fix :: (a->a) -> a
fix    f       = f (fix f)

ones' :: [Integer]
ones' = fix (1:)

nats' :: [Integer]
nats' = fix (map succ .) 0

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

{-
ackermann' :: Integer -> Integer -> Integer
ackermann' n m = fix 
-}

bottom :: a
bottom = fix bottom

findPlateau :: Eq a => [a] -> a
findPlateau a | head a == head (tail a) = head a
              | otherwise = findPlateau (tail a) 

tz :: Int -> Int
tz n = negate (n `div` 2)

mystery3_5 :: Eq a => (a -> a) -> a -> a
mystery3_5 = (findPlateau .) . iterate

{-Mystery 3_5:

returns an infinite list of repeated applications of f to x

Return list with true or false, with all true values meaning same
value is in the next position.

-}

mersenne :: [Int]
mersenne = map (\n -> n * n - 1) [0..]

eratosthenes :: [Int]
eratosthenes = sieve (map Just [2..])
  where
    sieve (Just n  : ns) = n : sieve (zipWith ($) mark_n ns)
      where mark_n = cycle (replicate (n-1) id ++ [const Nothing]) 
    sieve (Nothing : ns) = sieve ns
mersennePrime :: [Int]
mersennePrime = [1,2]

{-Look at solutions-}

{-Q4-}

newtype Tree a = Tree [(a, Tree a)] deriving Show

data VM_Event = Coin | Choc | Fudj | Halt deriving (Eq, Show)

type VM = Tree VM_Event

vm1_2 :: VM
vm1_2  = Tree [(Coin, Tree [(Choc, Tree [(Coin, Tree [(Choc, Tree [])])])])]

vm1_e :: VM
vm1_e = Tree [(Coin, Tree [(Choc, vm1_e)])] 

leadsto :: a -> Tree a -> Tree a
e `leadsto` b = Tree [(e, b)]

branch :: Tree a -> Tree a -> Tree a
Tree ts `branch` Tree us = Tree (ts ++ us)

vm1_2' :: VM
vm1_2' = Coin `leadsto` (Choc `leadsto` (Coin `leadsto` (Choc `leadsto` stop)))

stop :: Tree a
stop = Tree []

vm1_e' :: VM
vm1_e' = Coin `leadsto` vm1_e'

vm1_h :: VM
vm1_h = (Coin `leadsto` ((Choc `leadsto` vm1_h)
                         `branch`
                         (Halt `leadsto` stop)))
        `branch`
        (Halt `leadsto` stop)

vm1_h' :: VM
vm1_h' = Coin `leadsto` (Choc `leadsto` vm1_h')
        `branch`
        (Halt `leadsto` stop)

takeTree :: Int -> Tree a -> Tree a
takeTree = undefined