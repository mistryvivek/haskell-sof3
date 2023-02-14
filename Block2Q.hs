import Prelude

{- Any number that is even is added to one.-}
oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  even n]
oneone' [] = []
oneone' ns = if even (head ns)
             then (head ns + 1) : oneone' rest
             else oneone' rest
   where rest = tail ns

{- oneone' = map (+1) . filter ((==0) . (`mod` 2)) 

Mod 2 to all items in a list
Filter by checking whether the result is 0.
Return a list where you add +1 to each remaining element in the list.-}

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

{-
onetwo' = map f . filter g . concat . filter h
  where
    f = (== 1) . (`mod` 2) . fromEnum
    g = (`elem` ['a'..'z'])
    h = (>1) . length
    
first take list where the length is greater then 1 (filter h)
makes a 2d list into one dimension (concat . filter h)
ensures that the charactor is a lower case letter (g)
filters the 1d list using the function g to the output so far (filter g . concat . filter h)
Applies the function f to constructed list.
-}

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

{-Odd and even are pre defined definitions in prelude.-}

{-"Just" gives ePbs2iM, the expected data type.
Then ePbs2iM gives the bitstring if there is even parity.
Double odd m takes this input and double any odd number
Show takes the output.

If nothing is returned at any point, it will return "Ooops!"-}

doepM :: [Bool] -> String
doepM = maybe "Ooops!" show . doubleOddM . ePbs2iM . Just

type Error a = Either String a

ePbs2iE :: Error [Bool]          -> Error Int
ePbs2iE    (Left msg)             = Left msg
ePbs2iE    (Right bs) | parity bs = Right (bitstring2int bs)
                      | otherwise = Left "input has odd parity"

doubleOddE :: Error Int -> Error Int
doubleOddE  (Left msg) = Left msg 
doubleOddE  (Right o) | o `mod` 2 == 1 = Right (o * 2)
                      | otherwise = Left "Must be an odd number!"

doepE :: [Bool] -> String
doepE = either ("ERROR: "++) show . doubleOddE . ePbs2iE . Right

{-Q3.1:

Left to Right

(if True then inc else dbl) (2+1)
Inc (2+1)
(2+1) + 1
4-}

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

{- MODEL ANSWER:

nats' = fix ((0:) . map (1+))-}

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

{-GET FURTHER EXPLAINATION-}
ackermann' :: Integer -> Integer -> Integer
ackermann' = fix ack
  where
    ack _ 0 n = n+1
    ack f m 0 = f (m-1) 1
    ack f m n = f (m-1) (f m (n-1))

bottom :: a
bottom = fix id

findPlateau :: Eq a => [a] -> a
findPlateau a | head a == head (tail a) = head a
              | otherwise = findPlateau (tail a) 

{-
MODEL SOLUTION

findPlateau xs = head [x | (x, y) <- zip xs (tail xs), x==y]
Makes pairs with the tail and the whole list and maps onto the tuple (x,y), if x
and y are equal add it to the list. 
Takes the head which is the first element in the list (hence the first occurance
in the list).
-}

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

{-
MODEL SOLUTION: mersenne = [2^n - 1 | n<-[0..]]
-}

eratosthenes :: [Int]
eratosthenes = sieve (map Just [2..])
  where
    sieve (Just n  : ns) = n : sieve (zipWith ($) mark_n ns)
      where mark_n = cycle (replicate (n-1) id ++ [const Nothing]) 
    sieve (Nothing : ns) = sieve ns

{-
**Note** there is a function in `Data.List`, called `intersect` that
computes the intersection of a list and a **finite** list.  Hence it
will not work to compute the intersection in this case.  You will need
to generate the intersection by another method, taking advantage of
the fact that both infinite lists are ordered.
-}
mersennePrime :: [Int]
mersennePrime = eratosthenes `inter` mersenne
  where
    ee@(e:es) `inter` mm@(m:ms) | e < m     = es `inter` mm
                                | e == m    = e : es `inter` ms
                                | otherwise = ee `inter` ms

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
vm1_e' = Coin `leadsto` (Choc `leadsto` vm1_e')

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

{- Takes in input to a get to a given length "n".

Base case is where you reach the level you want to end at.
Stop just returns "Tree []"

Tree is stored in a list [] containing a tuple () - left side 
is always a leaf and the right side is always a Tree.

So you can use the data structure defined "ts" to construct 
a list containing a tuple which can then be converted to a 
"Tree" type.

So you keep on going to the right side of the tuple till you 
stop to get to that end of the tree.
-}
takeTree :: Int -> Tree a -> Tree a
takeTree 0 _         = stop
takeTree n (Tree ts) = Tree (map fn ts)
  where
    fn (e, t) = (e, takeTree (n - 1) t)

{-Similar logic to one before.

Ignore e because we dont need to read data.

ASK: Why use the sum function?-}
countTree :: Tree a -> Int
countTree (Tree ts) = sum (map fn ts)
  where
    fn (_, t) = 1 + countTree t

{-Replaces tree as follows:
Tree [(Coin, Tree [(Fudj, Tree [(Coin, Tree [(Fudj, Tree [])])])])]

Replacing using the function `length . show` gives:
Tree [(4, Tree [(4, Tree [(4, Tree [(4, Tree [])])])])]
-}
choc2fudj :: VM_Event -> VM_Event
choc2fudj    Choc      = Fudj
choc2fudj    x         = x

{- Saying take in a Tree at every leaf apply the function
passed in to the edge node.

Otherwise, keep on iterating till the root node is reached.

-}
mapTree :: (a -> b) -> Tree a   -> Tree b
mapTree    f         = rtf
  where
    rtf (Tree ts) = Tree (map fn ts)
    fn (e, t) = (f e, rtf t)

{-FURTHER BLOCK: Mapping over a type!-}

instance Functor Tree where
  fmap = mapTree
