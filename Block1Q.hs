{-
Q1. Evaluating Simple Expressions
1. 3+8=11
2. 3^2=9
3. 2^3^4==(2^3)^4 - FALSE
4. 2^3^4==2^(3^4) - TRUE
5. 0==1 || 0/=1 - TRUE
6: "h" : "ello" - charactor concat with string (Hello)
7. 7 : [6, 5, 4] - list concat
8. if 0==1 then "Alice" else "Bob" - Bob
9. let sqr = \ n -> n^2 in sqr 3 + sqr 4 - Using a Lamba function to define squares before doing something.
10. let cube n = n^3 in cube 2 + cube 3 - Using a function to define cubes and using it in an expression.

Q3. Detecting Errors
1.if 0 then "a" else "b" - if clause doesn't contain anything to be mapped to boolean.
2.if "a" == "b" then 5 - must use if/then/else pattern in Haskell.
3.if "a" = "b" then 5 else 4 - singleton equals doesn't work for equality.
4. if 0 == 0 then 5 else 'b' - must return an int or a string
5. ["0", 5, 9] - List of multiple types cannot exist.
6. ("a", 42 - Missing closing parenthesis.
7. let sumSq m n = m^2 - n^2 in sumSq 3 4 - Plus instead of the minus.
8. let f (x:xs) = 42 in f [] - need to define a pattern for the empty list.
9. let f n = if n == 0 then 0 else f (n+1) in f 3 - No reachable base case.
10. * 2 3 - Need bracket around the multiplication.
11. "3333" :: Int - Its a string!

Q4. Simple expressions that use values defined in Prelude
1. fst, snd - First and second value from given tuple.
2. curry, uncurry - Pack and unpack a tuple input.
3. succ, pred - Next and previous value (+/- 1: numeric values)
4. negate, abs, signum - change sign, get value w/o sign and
get sign.
5. quot, rem, div, mod, divMod - division related functions.
6. gcd - greater common divisor
7. id, const, (.) - returns itself
-}

import Prelude

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

greetTest :: Bool
greetTest
  = greet "Kofi" == "Hello Kofi!"
    && greet "Jeremy" == "Hello Jeremy!"
    && greet "" == "Hello !"

cakeBill :: Int -> Int -> String
cakeBill quantity price = "The cost of " ++ show quantity ++ " cakes at " ++
    show price ++ "p each is " ++ show (quantity * price) ++ "p."

cakeBillTest :: Bool
cakeBillTest =
  cakeBill 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill 1 3 == "The cost of 1 cakes at 3p each is 3p."
  && cakeBill 2 3 == "The cost of 2 cakes at 3p each is 6p."

cakeBill' :: Int -> Int -> String
cakeBill' quantity price =
  if quantity == 1
  then "The cost of " ++ show quantity ++ " cake at " ++
           show price ++ "p each is " ++ show (quantity * price) ++ "p."
  else "The cost of " ++ show quantity ++ " cakes at " ++
           show price ++ "p each is " ++ show (quantity * price) ++ "p."

cakeBill'Test :: Bool
cakeBill'Test =
  cakeBill' 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill' 1 3 == "The cost of 1 cake at 3p each is 3p."
  && cakeBill' 2 3 == "The cost of 2 cakes at 3p each is 6p."

bananas :: Int -> Int
bananas order | order < min_order = error "Must be 2kg or bigger"
              | ((order * 300) + 499) > 5000  = (order * 300) + (499 - 150)
              | otherwise         = (order * 300) + 499
    where
      min_order = 2

bananasTest :: Bool
bananasTest =
  bananas 2 == 1099
  && bananas 20 == 6349

pennies2pounds :: Int -> String
pennies2pounds x = "£" ++ show (fst tuple) ++ "." ++ show (snd tuple)
  where tuple = divMod x 100

implies :: Bool -> Bool -> Bool
implies True True = True;
implies a b = False;

data Item = Dog | Chicken | Grain deriving (Eq, Show)

eats :: Item -> [Item]
eats Chicken = [Grain]
eats Dog = [Chicken]
eats Grain = []

danger :: Item -> Item -> Bool
danger x y = elem y (eats x) || elem x (eats y)

incList :: [Int] -> [Int]
incList [] = []
incList (n:ns) = [n + 1] ++ incList ns

incList' :: [Int] -> [Int]
incList' a = map (+1) a

greetTest' = map (greet . fst) testData == map snd testData
  where testData = [("Kofi", "Hello Kofi!"),
                     ("Jeremy", "Hello Jeremy!"),
                     ("", "Hello !")]

pos :: String -> String -> Int
pos x y = if elem heady x
          then 0
          else 1 + pos x (tail y)
   where heady = head y

insert :: Ord a => a -> [a] -> [a]
insert b [] = [b]
insert b c = if b < head c
             then [b] ++ c
             else [head c] ++ insert b (tail c)

isort :: Ord a => [a] -> [a]
isort a = foldr insert [] a

insert' :: Ord a => a -> [a] -> [a]
insert' x = foldr
  where
    insx

----

product2 :: [Int] -> Int

product2 [] = 1
product2 (n:ns) = n * (product2 ns)

last2 :: [Int] -> Int

last2 n = reverse(n) !! 0

init2 :: [Int] -> [Int]
init2 n = reverse (tail (reverse n))

init3 :: [Int] -> [Int]
init3 (n:ns) = if length ns > 0
          then [n] ++ init3 ns
          else []

half2 :: [a] -> ([a],[a])
half2 a = (take num a, drop num a)
  where num = (length a) `div` 2