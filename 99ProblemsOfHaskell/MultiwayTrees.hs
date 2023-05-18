module MultiwayTrees where
import Data.Char (isAlpha)

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

stringToTree :: String -> Tree Char
stringToTree [] = Node 'a' []
stringToTree (x:xs) | isAlpha x = Node x [stringToTree xs]
    | otherwise = stringToTree xs

