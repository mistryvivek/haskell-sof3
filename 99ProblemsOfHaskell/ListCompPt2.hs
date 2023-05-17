module ListCompPt2 where

import Data.List(group)

data Set = Multiple Int Char | Single Char deriving Show

encodeModified :: [Char] -> [Set]
encodeModified = map fn . group
    where fn a | length a == 1 = Single (head a)
               | otherwise = Multiple (length a) (head a)

decodeModified :: [Set] -> [Char]
decodeModified = concat . map fn
    where fn (Multiple a b) = replicate a b 
          fn (Single b) = [b]

dupli :: [a] -> [a]
dupli = foldr (\x y -> replicate 2 x ++ y) []

repli :: Int -> [a] -> [a]
repli a = foldr (\x y -> replicate a x ++ y) []

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery num lis =  let (part, remainder) = splitAt num lis
             in init part ++ dropEvery num remainder

split' :: Int -> [a] -> ([a], [a])
split' num list = (take num list, drop num list)

slice :: [a] -> Int -> Int -> [a]
slice list num1 num2 = drop convIndex $ take num2 list
    where convIndex = num1 - 1

rotate :: [a] -> Int -> [a]
rotate list 0 = list
rotate list num = rotate (remainder ++ moveToEnd) (num - 1)
    where (moveToEnd, remainder) = splitAt 1 list 

removeAt :: Int -> [a] -> (a, [a])
removeAt num list = (list !! convIndex, take convIndex list ++ drop num list)
    where convIndex = num - 1


