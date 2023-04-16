module CanIHaskall where

import Prelude
import Data.Char (isDigit, digitToInt)
import Pack
    ( Value(Ace, Two), Suit(Spades, Clubs), Card(..), Hand )
import GHC.Utils.Encoding (zDecodeString)


isHaskell :: String -> Bool
isHaskell "Haskell" = True
isHaskell _ = False

testHaskell :: Bool
testHaskell = (isHaskell "Haskell" == True) &&
    (isHaskell "Software" == False) &&
    (isHaskell "" == False) &&
    (isHaskell "haskell" == False)

lowerVowel :: String -> Bool
lowerVowel s | null s = True
    | head s `elem` ['a', 'e', 'i', 'o', 'u'] = lowerVowel (tail s)
    | otherwise = False



testlv:: Bool
testlv = (lowerVowel "ue" == True) &&
    (lowerVowel "ueA" == False) &&
    (lowerVowel "uea" == True)

testprodCube :: Bool
testprodCube = (prodCube [] == 1)
    && (prodCube [4, 8] == 1)
    && (prodCube [4, 6, 8] == 216)
    && (prodCube [4, 6, 8, 12] == 216)
    && (prodCube [2..11] == 1728000)

prodCube :: [Int] -> Int
prodCube [] = 1
prodCube (x:xs) = if (x `mod` 2 == 0 && x `mod` 4 > 0)
                  then x^3 * prodCube xs 
                  else prodCube xs 

consDiff :: String -> Int
consDiff [] = 0
consDiff (x:xs) | x `elem` "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ" = 1 - consDiff xs
                | x `elem` "0123456789" = 1 + consDiff xs 
                | otherwise = consDiff xs

testconsDiff :: Bool
testconsDiff = (consDiff "" == 0)
    && (consDiff "SOf3in2021" == -2)
    && (consDiff "Software123andTheory123" == 5)
    && (consDiff "HASkellprogramming2021" == 9)

isIATA :: String -> Bool
isIATA a = length (filter (==True) (map (\x-> x `elem` "BCDFGHJKLMNPQRSTVWXYZ") a)) == 3

testisIATA :: Bool
testisIATA =
    (isIATA "" == False) &&
    (isIATA "MAN" == False) &&
    (isIATA "LHR" == True) &&
    (isIATA "LHRT" == False) &&
    (isIATA "lhr" == False) &&
    (isIATA "JFK" == True) &&
    (isIATA "BHX" == True)

testcountIATA :: Bool
testcountIATA =
    (countIATA ["LHR"] == 1) &&
    (countIATA ["LHR", "Lhr", "MAN", "JFK", "", "jfk"] == 2) &&
    (countIATA ["LHR", "BHX", "MAN", "JFK", "ACC", "LRHT"] == 3)

countIATA :: [String] -> Int
countIATA a = length (filter (== True) (map isIATA a))

type Students = [(Name, Age, College)]
type Name = String
type Age = Int
type College = String

onBus66 :: Students
onBus66 = [("Zain", 18, "Halifax"), ("Julia", 20, "Constantine"), ("Mandy", 22, "Goodricke"), ("Jack", 24, "Constantine"),("Emma", 21, "Langwith"), ("Zack", 19, "Halifax"),("Alice", 21, "Halifax"), ("Bob", 19, "Alcuin"),("Lui", 22, "Goodricke")]

colleges :: Age -> [College]
colleges a = [college | (_,age,college) <- onBus66, age == a]

type BoolD a = (Bool, a)

bd2m :: BoolD a -> Maybe a
bd2m (True, a) = Just a
bd2m (False, _) = Nothing

bDSum :: [Char] -> BoolD Int
bDSum ss = (True, sum [digitToInt s | s <- ss, isDigit s])

mBSum :: [Char] -> Maybe Int
mBSum ss = bd2m (bDSum ss)

data TreeP a = Leaf Int | Node (TreeP a) a (TreeP a) deriving (Eq, Show)
emptyTreeP :: TreeP a
emptyTreeP = Leaf 0

itp :: Ord a => a -> TreeP a -> TreeP a 
itp new (Node s value g) | value > new = Node (itp new s) value g
        | value < new = Node s value (itp new g)
itp new (Leaf a) = Node (Leaf newa) new (Leaf newa)
    where newa = a + 1

treeList :: TreeP a -> [a]
treeList (Node s value g) = treeList s ++ [value] ++ treeList g 
treeList (Leaf a) = [] 


data DBTree = DBLeaf | DBNode DBTree (Int, String) DBTree deriving (Eq, Show)


smallDB :: DBTree
smallDB = DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Helen") DBLeaf)


testUpdate :: Bool
testUpdate = (stdUpdate 6 "Abi" smallDB == Right (DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Abi") DBLeaf))) && (stdUpdate 8 "Mandy" smallDB == Left "There is no such student with ID: 8")




stdUpdate :: Int -> String -> DBTree -> Either String DBTree
stdUpdate id name tree = if tree == newTree
                         then Left "There is no such student with ID: 8"
                         else Right (helper id name tree)
    where 
        helper id new_name (DBNode a (comparision, old) b) | comparision == id = DBNode a (comparision, new_name) b
                                                    | comparision > id = DBNode (helper id new_name a) (comparision, old) b
                                                    | otherwise = DBNode a (comparision, old) (helper id new_name b)
        helper _ _ DBLeaf = DBLeaf
        newTree = helper id name tree

infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show

pack :: [Card]
{-COPIED-}
pack = [Card s v | s <- [Clubs .. Spades], v <- [Two .. Ace]]

psrnTest :: Bool
psrnTest = psrn 1234567890 == 395529916 && psrn 2468013579 == 1257580448

psrn :: Integral a => a -> a
psrn n = 7^5 * n `mod` (2^31 - 1)

insertMod :: Int -> [a] -> a -> [a]
insertMod n xs x = left ++ x:right
  where
    (left, right) = splitAt (n `mod` (length xs + 1)) xs

shuffleStep :: (Int -> Int) -> (Int, [a]) -> a -> (Int, [a])
shuffleStep a (num, list) c = (a num, insertMod num list c)

shuffleStepTest :: Bool
shuffleStepTest = shuffleStep id (3, "hello") 'x' == ( 3, "helxlo")
    && shuffleStep psrn (1234567890, [0 .. 4]) 9 == ( 395529916, [9,0,1,2,3,4])
    && shuffleStep psrn (2468013579, [0 .. 4]) 9 == (1257580448, [0,1,2,9,3,4])

shuffle :: (Int -> Int) -> Int -> [a] -> [a]
shuffle fn val ls = shuffleWithTracker fn val ls []

shuffleWithTracker :: (Int -> Int) -> Int -> [a] -> [a] -> [a]
shuffleWithTracker _ _ [] lsNew = lsNew
shuffleWithTracker fn val lsOld lsNew = shuffleWithTracker fn newValue updatedOldLs lsNewUpdated
    where nextToAdd = head lsOld
          updatedOldLs = tail lsOld
          (newValue, lsNewUpdated) = shuffleStep fn (val, lsNew) nextToAdd 

shuffleTest :: Bool
shuffleTest =
    shuffle psrn 1234567890 [0 .. 9] == [9,4,1,0,3,7,6,5,8,2]
        && shuffle psrn 2468013579 [0 .. 9] == [2,4,1,3,5,8,9,6,0,7]
        && shuffle id 0 [0 .. 9] == [9,8,7,6,5,4,3,2,1,0]
        && shuffle (+1) 0 [0 .. 9] == [0,1,2,3,4,5,6,7,8,9]

insertOrd :: Ord a => a -> [a] -> [a]
insertOrd w = foldr f [w]
    where f x ys@(z:zs) | x > w = z:x:zs
                        | otherwise = x:ys  

deal :: Ord a => [a] -> ([a], [a], [a], [a])
{-solution-}
deal = foldr dealAndTurn ([], [], [], [])
  where
    dealAndTurn c (ws, ss, es, ns) = (insertOrd c ns, ws, ss, es)

shuffleDeal  :: (Int -> Int)                -- function to update integer
                -> Int                      -- seed
                -> (Hand, Hand, Hand, Hand) -- result
{-solution-}
shuffleDeal next seed = deal (shuffle next seed pack)


type Trumps = Maybe Suit  

noTrumpSuit :: Trumps
noTrumpSuit = Nothing

theTrumpSuit :: Suit -> Trumps
theTrumpSuit = Just

trickWinner :: Trumps -> Card -> [Card] -> Card
trickWinner t (Card pack num) ls | t == noTrumpSuit = findHighestInPack pack
    where findHighestInPack pack = maximum [Card pack v `elem` ls| v <- [Two .. Ace]]

