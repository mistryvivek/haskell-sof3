import Prelude

{- Any number that is even is added to one.-}
oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  n `mod` 2 == 0 ]
oneone' [] = []
oneone' ns = if (head ns) `mod` 2 == 0
             then [head ns + 1] ++ oneone' rest
             else [] ++ oneone' rest
   where rest = tail ns

onetwo, onetwo' :: [String] -> [Bool]
onetwo css = [(fromEnum c) `mod` 2 == 1 |
              cs <- css, length cs > 1, c <- cs, c `elem` ['a'..'z']]