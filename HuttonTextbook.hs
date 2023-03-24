module HuttonTextbook where

{-1.1:

double (double 2) 
    :=: double 2 + double 2
    :=: double 2 + (2 + 2)
    :=: (2 + 2) + (2 + 2)
    :=: 4 + 4
    :=: 8

1.2: 

sum [x] 
    :=: x + sum[]
    :=: x + 0
    :=: x

1.5:

Remove duplicates.
-}

product' :: [Int] -> Int
product' = foldr (*) 1

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where 
                  smaller = [a | a <- xs, a <= x]
                  larger = [b| b <- xs, b > x]

n2 :: Int
n2 = a `div` length xs
    where 
        a = 10
        xs = [1,2,3,4,5]

last' :: [a] -> a
last' a = a !! (length a - 1)

init'One, init'Two :: [a] -> [a]
init'One a = take index a
    where index = length a - 1
init'Two a = reverse (tail (reverse a))

bools :: [Bool]
bools = map not [True, False, False]

nums :: [[Int]]
nums = [[1..3], [9..11], [12..18]]

add' :: Int -> Int -> Int -> Int
add' a b c = a + b + c

copy' :: a -> (a, a)
copy' a = (a, a)

halve :: [a] -> ([a], [a])
halve ls = splitAt (length ls `div` 2) ls

third, third', third'' :: [a] -> a
third = head. tail . tail 
third' a = a !! 2
third'' (_:_:x:_)  =  x

safeTail, safeTail', safeTail''  :: [a] -> [a]
safeTail a = if null a
           then []
           else tail a
safeTail' a | null a = []
            | otherwise = tail a
safeTail'' [] = []
safeTail'' a = tail a

sqaures :: Int
sqaures = sum [x^2|x<-[1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid a b = [(a', b')| a'<-[0..a], b'<-[0..b]]

square :: Int -> [(Int, Int)]
square a = [(a', b')| a'<-[0..a], b'<-[0..a], a' /= b']

replicate' :: Int -> a -> [a]
replicate' n x = [x| _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x<-[1..n], y<-[1..n] ,z<-[1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x|x<-[1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x|x<-[1..n], sum(factors x) == x] 

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

expo :: Int -> Int -> Int
expo n 0 = 1
expo n c = n * expo n (c-1)

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

map' :: Foldable t => (a -> b -> b) -> b -> t a -> b
map' f = foldr f  


filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

{-MONODS-}

data Tree a = Leaf | Node (Tree a ) a (Tree a) deriving Show
instance Functor Tree where
    fmap g Leaf = Leaf 
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)


   