module BinaryTrees where

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

{-

MY SOLN:

mirror :: Tree a -> Tree a
mirror (Branch x y _) = Branch x y (mirror y)
mirror Empty = Empty

symmetric :: Tree a -> Bool
symmetric ts = compare ts (mirror ts)
    where compare Empty Empty = True
          compare _ Empty = False
          compare Empty _ = False
          compare (Branch _ x1 y1) (Branch _ x2 y2) = compare x1 x2 && compare y1 y2
-}

--BETTER MODEL SOLN:

mirror :: Tree a -> Tree a -> Bool
mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _              _              = False

symmetric :: Tree a -> Bool
symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r

-- BINARY SEARCH TREE FORMAT: USED THE MODEL SOLN.
add :: Ord a => a -> Tree a -> Tree a
add x Empty            = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
                            LT -> Branch y (add x l) r
                            GT -> Branch y l (add x r)
                            EQ -> t

construct :: (Foldable t, Ord a) => t a -> Tree a
construct = foldl (flip add) Empty