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

greetTest' = ""
  where testData = [("Kofi", "Hello Kofi!"),
                     ("Jeremy", "Hello Jeremy!"),
                     ("", "Hello !")]

pos :: String -> String -> Int
pos x y = if elem heady x
          then 0
          else 1 + pos x (tail y)
   where heady = head y




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