module MultiwayTrees where

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

stringToTree :: String -> Tree Char
stringToTree [] = Node 't' []
stringToTree (x:xs) | x == '^' = Node x [] 
    | null xs = Node x []
    | otherwise = Node x [stringToTree xs]

