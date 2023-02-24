module CanIHaskall where

import Prelude
import Data.Char (isNumber)
import Data.ByteString (count)

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
                | isNumber x = 1 + consDiff xs 
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

colleges = map getCollege onBus66
    where getCollege (_, x, _) = x
