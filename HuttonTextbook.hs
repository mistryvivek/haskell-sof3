module HuttonTextbook where
import Control.Applicative (ZipList)

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

bools' :: [Bool]
bools' = map not [True, False, False]

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

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf = Leaf 
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x| x <- xs]

multi :: Int -> Int -> Int -> Int
multi = \z -> \y -> (\x -> x*y*z)

luhnDouble :: Int -> Int
luhnDouble x | doubleVal > 9 = doubleVal - 9
    | otherwise = doubleVal
    where doubleVal = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b x c = (luhnDouble a + luhnDouble b + x + luhnDouble c) `mod` 10 == 0

reexpress :: (a -> b) -> (a -> Bool) -> [a] -> [b]
reexpress x p = map x . filter p

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> y + x*10) 0 

curry' :: ((a, b) -> c) -> a -> b -> c
curry' fn = \x y -> fn (x, y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' fn = \(x,y) -> fn x y

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x:[])   = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

occurs :: Ord a => a -> Tree a -> Bool
occurs a Leaf = False
occurs a (Node x y z) | y == a = True
    | y > a = occurs a x
    | otherwise = occurs a z

data TreeTwo a = Leaf' a | Node' (TreeTwo a) (TreeTwo a) deriving Show

balanced :: TreeTwo a -> Bool
balanced (Node' x y) = abs (nodeCounter x - nodeCounter y) <= 1 && balanced x && balanced y
    where
        nodeCounter (Leaf' a) = 1
        nodeCounter (Node' x y) = nodeCounter x + nodeCounter y

halveList :: [a] -> ([a],[a])
halveList xs = splitAt (length xs `div` 2) xs

balance :: [a] -> TreeTwo a
balance [x] = Leaf' x
balance xs  = Node' (balance ys) (balance zs)
              where (ys,zs) = halveList xs

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval'' :: Expr -> Int
eval'' = folde id (+) 

size' :: Expr -> Int 
size' = folde (const 1) (+)

data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
    (==) :: Eq a => Maybe' a -> Maybe' a -> Bool
    (==) (Just' x) (Just' y) = x == y
    (==) Nothing' Nothing' = True
    (==) _ _ = False
 
data Prop = Const Bool 
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop
    | Or Prop Prop
    | Equiv Prop Prop 

type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equiv p q) = eval s p || eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q 
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

data Expr = Val Int | Add Expr Expr | Multi Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Multi x y) = value x * value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int | MULTI Int

