module ListComp where
    
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
flatten = map fn 
    where fn (List x) = map fn x
          fn (Elem x) = x 