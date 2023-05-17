module ListCompPt3 where

import System.Random
import Data.List (tails, nub, sort, sortOn, groupBy)

insertAt :: Int -> [a] -> a -> [a]
insertAt num lis eleme =  start ++ [eleme] ++ end
    where (start, end) = splitAt (num - 1) lis

range :: Int -> Int -> [Int]
range a b = [a..b]

-- N.B. RANDOM IS NOT A PART OF THE BASE PACKAGE.
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

diff_select :: Int -> Int -> IO[Int]
diff_select a b = rnd_select [1..b] a

rnd_permu :: [a] -> IO[a]
rnd_permu xs = rnd_select xs (length xs)
-- END OF OUT OF SCOPE QUESTIONS.


-- Using list comprehensions
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs , ys <- combinations (n-1) xs']

group sizes xs = factorial (length xs) -- `div` product $ map factorial sizes 
    where factorial end = product [1..end]

-- Come back to problem 27.

lsort :: [[a]] -> [[a]]
lsort = sortOn length
 
lfsort :: [[a]] -> [[a]]
lfsort lists = concat groups
    where groups = lsort $ groupBy equalLength $ lsort lists
          equalLength xs ys = length xs == length ys
