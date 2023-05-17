module ListComp where

import Data.List(group)

myLast', myLast :: [a] -> a
myLast' = head . reverse
myLast = last

myButLast' :: [c] -> c
myButLast' = head . tail . reverse

elementAt :: Int -> [a] -> Maybe a
elementAt num items | length items < num = Nothing 
    | num <= 0 = Nothing 
    | otherwise = Just $ items !! (num - 1)

myLength :: [a] -> Int
myLength = length 

myReverse :: [a] -> [a]
myReverse = reverse

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs        

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x] 
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
-- DOES IT THE WRONG WAY ROUND: compress = foldr (\x y -> if x `elem` y then y else x : y) [] 
compress = map head . group 

encode :: Eq a => [a] -> [(Int, a)]
encode = map fn . group
    where fn x = (length x, head x)

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []